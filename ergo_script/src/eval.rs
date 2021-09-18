//! Script runtime evaluation.
//!
//! The runtime is responsible for evaluating AST expressions, producing values. Importantly, it
//! tracks source locations for values so that when an error occurs, useful error information can
//! be provided.

use crate::ast::{self, CaptureKey, CaptureSet, DocCommentPart, Expr};
use ergo_runtime::abi_stable::external_types::RMutex;
use ergo_runtime::Result;
use ergo_runtime::{
    depends,
    metadata::{self, Source},
    nsid, traits, try_result, types,
    value::match_value,
    Context, Dependencies, Value,
};
use futures::future::{join_all, BoxFuture, FutureExt};
use log::error;
use std::collections::BTreeMap;
use std::sync::atomic::AtomicUsize;

pub const EVAL_TASK_PRIORITY: u32 = 100;

#[derive(Clone, Copy, Debug, Default)]
pub struct Evaluator {
    pub backtrace: bool,
}

#[derive(Debug)]
pub enum Capture {
    Expr {
        expression: Expr,
        captures_left: AtomicUsize,
        needed_by: CaptureSet,
    },
    Needed {
        needed_by: CaptureSet,
    },
    Evaluated(Value),
}

impl Clone for Capture {
    fn clone(&self) -> Self {
        match self {
            Capture::Expr {
                expression,
                captures_left,
                needed_by,
            } => Capture::Expr {
                expression: expression.clone(),
                captures_left: AtomicUsize::new(
                    captures_left.load(std::sync::atomic::Ordering::Relaxed),
                ),
                needed_by: needed_by.clone(),
            },
            Capture::Needed { needed_by } => Capture::Needed {
                needed_by: needed_by.clone(),
            },
            Capture::Evaluated(v) => Capture::Evaluated(v.clone()),
        }
    }
}

impl Default for Capture {
    fn default() -> Self {
        Capture::Needed {
            needed_by: Default::default(),
        }
    }
}

impl Capture {
    fn add_needed(&mut self, key: CaptureKey) {
        match self {
            Capture::Expr { needed_by, .. } | Capture::Needed { needed_by } => {
                needed_by.insert(key)
            }
            _ => (),
        }
    }

    pub fn needs(&self) -> Option<&CaptureSet> {
        match self {
            Capture::Expr { expression, .. } => expression.captures(),
            _ => None,
        }
    }
}

impl From<&Capture> for Dependencies {
    fn from(capture: &Capture) -> Self {
        match capture {
            Capture::Expr { expression, .. } => depends![expression],
            Capture::Evaluated(v) => depends![v],
            Capture::Needed { .. } => depends![],
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Captures {
    // The use of a BTreeMap is important so that dependencies can incorporate the relative capture key
    // order such that dependencies that swap values will end up differently (without depending on the
    // capture key values themselves, which may change from unrelated code).
    inner: BTreeMap<CaptureKey, Capture>,
    ready: CaptureSet,
}

impl std::iter::FromIterator<(CaptureKey, (Expr, CaptureSet))> for Captures {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (CaptureKey, (Expr, CaptureSet))>,
    {
        let mut ready = CaptureSet::default();

        let (mut inner, needs): (BTreeMap<_, _>, Vec<_>) = iter
            .into_iter()
            .map(|(k, (expression, needs))| {
                let captures_left = needs.len();
                if captures_left == 0 {
                    ready.insert(k);
                }
                let entry = (
                    k,
                    Capture::Expr {
                        expression,
                        captures_left: captures_left.into(),
                        needed_by: Default::default(),
                    },
                );
                (entry, (k, needs))
            })
            .unzip();
        // Add backward needs
        for (k, needs) in needs {
            for n in needs.iter() {
                inner.entry(n).or_default().add_needed(k);
            }
        }
        Captures { inner, ready }
    }
}

impl From<&Captures> for Dependencies {
    fn from(captures: &Captures) -> Dependencies {
        depends![^captures.values()]
    }
}

impl Captures {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn values(&self) -> std::collections::btree_map::Values<'_, CaptureKey, Capture> {
        self.inner.values()
    }

    pub fn get(&self, key: CaptureKey) -> Option<&Value> {
        self.inner.get(&key).and_then(|cap| match cap {
            Capture::Evaluated(val) => Some(val),
            _ => None,
        })
    }

    pub fn subset(&self, subset: &CaptureSet) -> Self {
        let mut ret = Captures::new();
        let mut to_check: CaptureSet = self
            .inner
            .keys()
            .filter(|k| subset.contains(**k))
            .copied()
            .collect();
        while let Some(k) = to_check.pop() {
            if !ret.inner.contains_key(&k) {
                if let Some(v) = self.inner.get(&k) {
                    ret.inner.insert(k.clone(), v.clone());
                    if let Some(set) = v.needs() {
                        to_check.union_with(set);
                    }
                }
            }
        }
        ret
    }

    pub fn resolve(&mut self, key: CaptureKey, value: Value) {
        if let Some(v) = self.inner.insert(key, Capture::Evaluated(value)) {
            match v {
                Capture::Evaluated(_) => panic!("capture resolved more than once"),
                Capture::Expr { needed_by, .. } | Capture::Needed { needed_by, .. } => {
                    for key in needed_by.iter() {
                        if let Some(Capture::Expr { captures_left, .. }) = self.inner.get_mut(&key)
                        {
                            if captures_left.fetch_sub(1, std::sync::atomic::Ordering::Relaxed) == 1
                            {
                                self.ready.insert(key);
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn resolve_string_gets(&mut self, with: BTreeMap<String, Value>) -> Result<()> {
        let mut resolves = vec![];
        for (k, c) in self.inner.iter_mut() {
            if let Capture::Expr { expression, .. } = c {
                if let Some(get) = expression.value().as_ref::<ast::Get>() {
                    if let Some(s) = (*get.value).as_ref::<ast::String>() {
                        match with.get(s.0.as_str()) {
                            Some(r) => {
                                resolves.push((*k, r.clone()));
                            }
                            None => {
                                return Err(get.value.source().with("unbound value").into_error())
                            }
                        }
                    }
                }
            }
        }
        for (k, r) in resolves {
            self.resolve(k, r);
        }
        Ok(())
    }

    pub fn evaluate_ready<'a>(&'a mut self, eval: Evaluator) -> BoxFuture<'a, ()> {
        async move {
            while self.ready.len() > 0 {
                let ready = std::mem::take(&mut self.ready);
                let results =
                    {
                        let me: &Self = self;
                        join_all(ready.iter().filter_map(|key| {
                            if let Some(Capture::Expr { expression, .. }) = me.inner.get(&key) {
                                Some(async move {
                                    (key, eval.evaluate_now(expression.clone(), me).await)
                                })
                            } else {
                                None
                            }
                        }))
                        .await
                    };
                for (k, r) in results {
                    self.resolve(k, r);
                }
            }
        }
        .boxed()
    }
}

type LocalEnv = BTreeMap<Value, Value>;
type SetsItem = (Option<CaptureKey>, Value, Value);

#[derive(Clone)]
struct Sets {
    inner: Option<std::sync::Arc<RMutex<Vec<SetsItem>>>>,
}

impl std::fmt::Debug for Sets {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.inner {
            None => write!(f, "None"),
            Some(a) => write!(f, "{:?}", a.lock()),
        }
    }
}

impl Sets {
    pub fn new() -> Self {
        Sets {
            inner: Some(std::sync::Arc::new(RMutex::new(vec![]))),
        }
    }

    pub fn none() -> Self {
        Sets { inner: None }
    }

    pub fn is_present(&self) -> bool {
        self.inner.is_some()
    }

    pub fn add(&self, cap: Option<CaptureKey>, key: Value, value: Value) {
        self.inner().push((cap, key, value));
    }

    pub fn into_inner(self) -> Vec<SetsItem> {
        match std::sync::Arc::try_unwrap(self.inner.expect("internal error: set scope missing")) {
            Err(a) => {
                error!("Internal error: set scope unexpectedly persisted. This is likely caused by ValueByContent being incorrectly implemented.");
                a.lock().clone()
            }
            Ok(v) => v.into_inner(),
        }
    }

    pub fn extend<T: IntoIterator<Item = SetsItem>>(&self, sets: T) {
        let mut guard = self.inner();
        for entry in sets.into_iter() {
            guard.push(entry);
        }
    }

    fn inner(
        &self,
    ) -> ergo_runtime::abi_stable::external_types::parking_lot::mutex::RMutexGuard<Vec<SetsItem>>
    {
        self.inner
            .as_ref()
            .expect("internal error: set scope missing")
            .lock()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockItemMode {
    Block,
    Command,
    DocBlock,
}

impl BlockItemMode {
    fn command(&self) -> bool {
        self == &BlockItemMode::Command
    }

    fn doc(&self) -> bool {
        self == &BlockItemMode::DocBlock
    }
}

impl Evaluator {
    /// Evaluate the given expression.
    pub fn evaluate(self, e: Expr, captures: &Captures) -> Value {
        self.evaluate_with_env(e, captures, None, &Sets::none())
    }

    /// Evaluate the given expression immediately.
    pub async fn evaluate_now(self, e: Expr, captures: &Captures) -> Value {
        self.evaluate_now_with_env(e, captures, None, &Sets::none())
            .await
    }

    /// Evaluate block items.
    ///
    /// Returns the vector of normal expressions in the block, the env formed from all bindings
    /// within the block, and whether the block ended with a normal expression.
    async fn evaluate_block_items(
        self,
        items: Vec<ast::BlockItem>,
        mut captures: Captures,
        mode: BlockItemMode,
        sets: &Sets,
    ) -> Result<(Vec<Value>, LocalEnv)> {
        let mut env = LocalEnv::new();
        let mut results = Vec::new();
        let mut errs = Vec::new();
        let mut had_unbound = false;
        let mut has_errors = false;

        async fn push_result(
            mode: BlockItemMode,
            results: &mut Vec<Value>,
            v: Value,
            last: bool,
        ) -> Result<()> {
            if mode.command() || last {
                results.push(v);
                Ok(())
            } else {
                traits::eval_nested(v).await
            }
        }

        let mut items = items.into_iter().peekable();

        while let Some(i) = items.next() {
            let last_item = items.peek().is_none();
            match i {
                ast::BlockItem::Expr(e) => {
                    push_result(
                        mode,
                        &mut results,
                        self.eval_with_env(
                            e,
                            &captures,
                            if mode.command() { None } else { Some(&env) },
                            sets,
                        )
                        .await,
                        last_item,
                    )
                    .await?;
                }
                ast::BlockItem::Merge(e) => {
                    let mut val = self
                        .evaluate_now_with_env(
                            e,
                            &captures,
                            if mode.command() { None } else { Some(&env) },
                            sets,
                        )
                        .await;
                    let val_source = Source::get(&val);
                    drop(Context::eval(&mut val).await);
                    match_value! { val.clone(),
                        s@types::String(_) => {
                            env.insert(Source::imbue(val_source.clone().with(s.into())), Source::imbue(val_source.with(types::Unit.into())));
                        }
                        types::Array(arr) => {
                            // TODO should this implicitly evaluate to a unit type when the array is
                            // empty?
                            let mut arr = arr.into_iter().peekable();
                            while let Some(v) = arr.next() {
                                let last_arr = arr.peek().is_none();
                                push_result(mode, &mut results, v, last_item && last_arr).await?;
                            }
                        }
                        types::Map(map) => {
                            env.extend(map);
                        }
                        types::Unbound {..} => {
                            if mode.command() {
                                results.push(Source::imbue(val_source.with(types::BindRest(val).into())));
                            } else {
                                if had_unbound {
                                    has_errors = true;
                                    errs.push(val_source.with("cannot have multiple unbound merges").into_error());
                                } else {
                                    env.insert(Source::imbue(val_source.clone().with(types::BindRestKey.into())), val);
                                    had_unbound = true;
                                }
                            }
                        }
                        types::Args { mut args } => {
                            if mode.command() {
                                results.extend(&mut args);
                                env.extend(args.keyed);
                            } else {
                                has_errors = true;
                                errs.push(val_source.with("cannot merge value with type Args").into_error());
                            }
                        }
                        types::PatternArgs { mut args } => {
                            if mode.command() {
                                results.extend(&mut args);
                                env.extend(args.keyed);
                            } else {
                                has_errors = true;
                                errs.push(val_source.with("cannot merge value with type PatternArgs").into_error());
                            }
                        }
                        e@types::Error {..} => {
                            has_errors = true;
                            errs.push(e);
                        }
                        v => {
                            has_errors = true;
                            let name = traits::type_name(&v);
                            errs.push(val_source.with(format!("cannot merge value with type {}", name)).into_error());
                        }
                    }
                }
                ast::BlockItem::Bind(b, e) => {
                    let e = self
                        .eval_with_env(
                            e,
                            &captures,
                            if mode.command() { None } else { Some(&env) },
                            sets,
                        )
                        .await;

                    let new_sets = Sets::new();
                    let b = self
                        .evaluate_now_with_env(
                            b,
                            &captures,
                            if mode.command() { None } else { Some(&env) },
                            &new_sets,
                        )
                        .await;

                    // Immediately eval and check for bind error
                    traits::bind_no_error(b, e).await?;
                    let new_sets = new_sets.into_inner();

                    if mode.doc() && sets.is_present() {
                        sets.extend(new_sets.clone());
                    }

                    for (cap, k, mut v) in new_sets {
                        Context::eval_once(&mut v).await;

                        env.insert(k, v.clone());
                        if let Some(cap) = cap {
                            captures.resolve(cap, v);
                        }
                    }
                    captures.evaluate_ready(self).await;
                }
            }
        }

        if has_errors {
            Err(types::Error::aggregate(errs))
        } else {
            Ok((results, env))
        }
    }

    async fn eval_with_env(
        self,
        e: Expr,
        captures: &Captures,
        local_env: Option<&LocalEnv>,
        sets: &Sets,
    ) -> Value {
        self.evaluate_with_env(e, captures, local_env, sets)
    }

    /// Evaluate the given expression with an environment.
    fn evaluate_with_env(
        self,
        e: Expr,
        captures: &Captures,
        local_env: Option<&LocalEnv>,
        sets: &Sets,
    ) -> Value {
        let (source, e) = e.take();

        let mut val = crate::match_expression!(e,
            Unit => |_| types::Unit.into(),
            BindAny => |_| types::Unbound::new_no_doc(
                            |_| async { types::Unit.into() }.boxed(),
                            depends![nsid!(expr::any)],
                        )
                        .into(),
            String => |s| types::String::from(s.0.clone()).into(),
            Force => |_| {
                panic!("unexpected force expression");
            },
            DocComment => |doc| {
                let mut captures = captures.subset(&doc.captures);
                let mut val = self.evaluate_with_env(doc.value.clone(), &captures, local_env, sets);
                let parts = doc.parts.clone();
                let self_key = doc.self_capture_key.as_ref().copied().unwrap();
                let self_val = val.clone();
                // TODO distinguish doc comment captures from value captures?
                let doc_deps = depends![^DocCommentPart::dependencies(&parts), ^&captures, self_val];
                let mut doc = Value::dyn_new(move || async move {
                        Context::spawn(EVAL_TASK_PRIORITY, |_| {}, async move {
                            ergo_runtime::error_info! {
                                labels: [ primary(source.with("while evaluating this doc comment")) ],
                                async {
                                    captures.resolve(self_key, self_val);
                                    captures.evaluate_ready(self).await;
                                    let mut doc = std::string::String::new();
                                    let mut formatter = traits::Formatter::new(&mut doc);
                                    let mut local_env = LocalEnv::new();
                                    for p in parts {
                                        match p {
                                            DocCommentPart::String(s) => formatter.write_str(&s)?,
                                            DocCommentPart::ExpressionBlock(es) => {
                                                // Evaluate as a block, displaying the final value and
                                                // merging the scope into the doc comment scope.
                                                let sets = Sets::new();
                                                let (mut vals, _) = self.evaluate_block_items(es, captures.clone(), BlockItemMode::DocBlock, &sets).await?;
                                                debug_assert!(vals.len() < 2);

                                                for (cap, k, v) in sets.into_inner() {
                                                    if let Some(cap) = cap {
                                                        captures.resolve(cap, v.clone());
                                                    }
                                                    local_env.insert(k, v);
                                                }
                                                captures.evaluate_ready(self).await;

                                                // Only add to string if the last value wasn't a bind
                                                // expression (to support using a block to only add
                                                // bindings).
                                                if let Some(mut last) = vals.pop() {
                                                    Context::eval(&mut last).await?;
                                                    traits::display(last, &mut formatter).await?;
                                                }
                                            }
                                        }
                                    }
                                    drop(formatter);
                                    Result::Ok(Value::from(types::String::from(doc)))
                                }
                            }
                        }).await.into()
                    }, doc_deps);
                Source::set(&mut doc, source.clone());
                metadata::Doc::set(&mut val, doc);
                val
            },
            Capture => |capture| match captures.get(capture.0) {
                None => {
                    if cfg!(debug_assertions) {
                        use ergo_runtime::error::{Diagnostic, DiagnosticInfo, diagnostics_to_string};
                        let d = Diagnostic::from(format!("internal capture error: {:?}", capture.0))
                            .add_primary_label(source.clone().with(""));
                        panic!("{}", diagnostics_to_string(&[d], Context::global().diagnostic_sources().as_ref(), false));
                    } else {
                        types::Unset.into()
                    }
                }
                Some(v) => v.clone()
            },
            Function => |func| {
                let captures = captures.subset(&func.captures);
                let deps = depends![e, ^&captures];
                let bind = func.bind.clone();
                let body = func.body.clone();
                types::Unbound::new_no_doc(move |v| {
                    let bind = bind.clone();
                    let body = body.clone();
                    let mut captures = captures.clone();
                    async move {
                        let sets = Sets::new();
                        let bind = self.evaluate_now_with_env(bind, &captures, None, &sets).await;
                        try_result!(traits::bind_no_error(bind, v).await);

                        let mut sets = sets.into_inner();

                        // The sets values will each evaluate to the bound value; we want to
                        // evaluate them immediately to properly get value identities.
                        Context::global().task.join_all((&mut sets).iter_mut().map(|s| async move {
                            Context::eval_once(&mut s.2).await;
                            Ok(())
                        })).await.unwrap();

                        let (new_captures, local_env): (Vec<_>, LocalEnv) = sets.into_iter().map(|(cap, k, v)| {
                            (cap.map(|c| (c, v.clone())), (k, v))
                        }).unzip();
                        for (cap, v) in new_captures.into_iter().filter_map(|o| o) {
                            captures.resolve(cap, v);
                        }
                        captures.evaluate_ready(self).await;

                        self.eval_with_env(body, &captures, Some(&local_env), &Sets::none()).await
                    }.boxed()
                }, deps).into()
            },
            Get => |get| {
                let k = self.evaluate_with_env(get.value.clone(), captures, local_env, sets);
                match local_env.and_then(|env| env.get(&k)) {
                    None => Source::get(&k).with("missing binding").into_error().into(),
                    Some(v) => v.clone()
                }
            },
            Set => |set| {
                let k = self.evaluate_with_env(set.value.clone(), captures, local_env, sets);
                let (send_result, receive_result) = futures::channel::oneshot::channel::<Value>();

                let receive_result = receive_result.shared();

                let mut v = Value::dyn_new(|| async move {
                    match receive_result.await {
                        Ok(v) => v,
                        Err(_) => types::Unset.into()
                    }
                }, depends![nsid!(ergo::get), k]);
                Source::set(&mut v, Source::get(&k));
                sets.add(set.capture_key.clone(), k.clone(), v);

                let send_result = std::sync::Arc::new(std::sync::Mutex::new(Some(send_result)));
                types::Unbound::new_no_doc(move |v| {
                    let send_result = send_result.lock().map(|mut g| g.take()).unwrap_or(None);
                    async move {
                        if let Some(sender) = send_result {
                            drop(sender.send(v));
                            types::Unit.into()
                        } else {
                            Source::get(&v).with("cannot bind a setter more than once").into_error().into()
                        }
                    }.boxed()
                }, depends![nsid!(ergo::set), k]).into()
            },
            _ => {
                let val_captures = e
                    .captures()
                    .map(|caps| captures.subset(caps))
                    .unwrap_or_default();
                let deps = depends![e, ^&val_captures];
                let e = source.clone().with(e);
                let sets = sets.clone();
                Value::dyn_new(move || async move {
                    Context::spawn(EVAL_TASK_PRIORITY, |_| {}, async move {
                        Ok(self.evaluate_now_with_env(e, &val_captures, None, &sets).await)
                    }).await.into()
                }, deps)
            }
        );

        Source::set_if_missing(&mut val, source);
        val
    }

    async fn evaluate_now_with_env(
        self,
        e: Expr,
        captures: &Captures,
        local_env: Option<&LocalEnv>,
        sets: &Sets,
    ) -> Value {
        let _src = e.source();
        let v = self
            .evaluate_now_with_env_impl(e, captures, local_env, sets)
            .await;
        if self.backtrace {
            match v.as_type::<types::Error>() {
                // FIXME reimplement backtrace in a different way
                Ok(err) => err.into(),
                Err(v) => v,
            }
        } else {
            v
        }
    }

    /// Evaluate the given expression immediately with an environment.
    fn evaluate_now_with_env_impl<'a>(
        self,
        e: Expr,
        captures: &'a Captures,
        local_env: Option<&'a LocalEnv>,
        sets: &'a Sets,
    ) -> BoxFuture<'a, Value> {
        async move {
            let (source, e) = e.take();

            let source_c = source.clone();

            crate::match_expression!(e,
                Unit => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                BindAny => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                String => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                Force => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                DocComment => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                Capture => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                Function => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                Get => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                Set => |_| self.eval_with_env(source.with(e), captures, local_env, sets).await,
                _ => {
                    let mut val = crate::match_expression!(e,
                        Array => |arr| {
                            let mut results = Vec::new();
                            let mut has_errors = false;
                            let mut errs = Vec::new();
                            for i in arr.items.clone() {
                                match i {
                                    ast::ArrayItem::Expr(e) => {
                                        results.push(self.eval_with_env(e, captures, None, sets).await);
                                    }
                                    ast::ArrayItem::Merge(e) => {
                                        let mut val = self.eval_with_env(e, captures, None, sets).await;
                                        let val_source = Source::get(&val);
                                        drop(Context::eval(&mut val).await);
                                        match_value! { val.clone(),
                                            types::Array(arr) => results.extend(arr),
                                            types::Unbound {..} => {
                                                results.push(Source::imbue(val_source.clone().with(types::BindRest(val).into())));
                                            }
                                            e@types::Error {..} => {
                                                has_errors = true;
                                                errs.push(e);
                                            }
                                            v => {
                                                has_errors = true;
                                                let name = traits::type_name(&v);
                                                errs.push(val_source.with(format!("cannot merge value with type {}", name)).into_error());
                                            }
                                        }
                                    }
                                }
                            }
                            if has_errors {
                                types::Error::aggregate(errs).into()
                            } else {
                                types::Array(results.into()).into()
                            }
                        },
                        Block => |block| {
                            match self.evaluate_block_items(block.items.clone(), captures.clone(), BlockItemMode::Block, sets).await {
                                Err(e) => e.into(),
                                Ok((mut vals, env)) => {
                                    debug_assert!(vals.len() < 2);
                                    match vals.pop() {
                                        Some(last) => last,
                                        None => {
                                            // Remove any Unset values
                                            types::Map(env.into_iter().filter(|(_,v)| !v.is_type::<types::Unset>()).collect()).into()
                                        }
                                    }
                                }
                            }
                        },
                        Index => |ind| {
                            let mut value = self.eval_with_env(ind.value.clone(), &captures, None, sets).await;
                            let index = self.eval_with_env(ind.index.clone(), &captures, None, sets).await;
                            if let Err(e) = Context::eval(&mut value).await {
                                let mut val = e.into();
                                Source::set_if_missing(&mut val, Source::get(&value));
                                return val;
                            }
                            traits::bind(value, Source::imbue(source_c.with(types::Index(index).into()))).await
                        },
                        Command => |cmd| {
                            let src = cmd.function.source();
                            let mut function = self.eval_with_env(cmd.function.clone(), &captures, None, sets).await;
                            match self
                                .evaluate_block_items(cmd.args.clone(), captures.clone(), BlockItemMode::Command, sets)
                                .await
                            {
                                Err(e) => e.into(),
                                Ok((pos, keyed)) => {
                                    if let Err(e) = Context::eval(&mut function).await {
                                        let mut val = e.into();
                                        Source::set_if_missing(&mut val, src);
                                        return val;
                                    }
                                    let args = types::args::Arguments::new(pos, keyed).unchecked();
                                    traits::bind(function, Source::imbue(source_c.with(types::Args { args }.into()))).await
                                }
                            }
                        },
                        PatternCommand => |cmd| {
                            let src = cmd.function.source();
                            let mut function = self.eval_with_env(cmd.function.clone(), &captures, None, sets).await;
                            match self
                                .evaluate_block_items(cmd.args.clone(), captures.clone(), BlockItemMode::Command, sets)
                                .await
                            {
                                Err(e) => e.into(),
                                Ok((pos, keyed)) => {
                                    if let Err(e) = Context::eval(&mut function).await {
                                        let mut val = e.into();
                                        Source::set_if_missing(&mut val, src);
                                        return val;
                                    }
                                    let args = types::args::Arguments::new(pos, keyed).unchecked();
                                    let pat_args = Source::imbue(source_c.with(types::PatternArgs { args }.into()));
                                    // Pattern commands have all arguments evaluated immediately in
                                    // debug builds to ensure that any set expressions are
                                    // evaluated and properly added to the capture scope.
                                    if cfg!(debug_assertions) {
                                        try_result!(traits::eval_nested(pat_args.clone()).await);
                                    }
                                    traits::bind(function, pat_args).await
                                }
                            }
                        },
                        _ => panic!("unexpected expression")
                    );

                    Source::set_if_missing(&mut val, source);
                    val
                }
            )
        }.boxed()
    }
}
