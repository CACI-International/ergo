//! Script runtime evaluation.
//!
//! The runtime is responsible for evaluating AST expressions, producing values. Importantly, it
//! tracks source locations for values so that when an error occurs, useful error information can
//! be provided.

use crate::ast::{self, CaptureKey, CaptureSet, Expr, ScopeKey, Subexpressions};
use cachemap::CacheMap;
use ergo_runtime::abi_stable::external_types::RMutex;
use ergo_runtime::Result;
use ergo_runtime::{
    depends,
    error::ErrorOrDiagnostic,
    metadata::{self, Source},
    nsid, traits, try_result, types,
    value::{match_value, ValueId},
    Context, EvaluatedValue, Value,
};
use futures::future::{BoxFuture, FutureExt};
use std::collections::{BTreeMap, HashMap};

pub const EVAL_TASK_PRIORITY: u32 = 100;

#[derive(Clone, Copy, Debug, Default)]
pub struct Evaluator {
    pub backtrace: bool,
}

#[derive(Clone, Debug)]
pub enum Capture {
    Expr(Expr),
    Evaluated(Value),
}

#[derive(Clone, Debug, Default)]
pub struct Captures {
    inner: HashMap<CaptureKey, Capture>,
}

impl std::iter::FromIterator<(CaptureKey, Expr)> for Captures {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (CaptureKey, Expr)>,
    {
        Captures {
            inner: iter
                .into_iter()
                .map(|(k, e)| (k, Capture::Expr(e)))
                .collect(),
        }
    }
}

impl Captures {
    pub fn get(&self, key: CaptureKey) -> Option<&Value> {
        self.inner.get(&key).and_then(|cap| match cap {
            Capture::Evaluated(val) => Some(val),
            _ => None,
        })
    }

    pub fn subset(&self, subset: &CaptureSet) -> Self {
        Captures {
            inner: self
                .inner
                .iter()
                .filter_map(|(k, v)| {
                    if subset.contains(*k) {
                        Some((*k, v.clone()))
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }

    pub fn ready(&self, subset: &CaptureSet) -> bool {
        subset.iter().all(|k| self.get(k).is_some())
    }

    pub fn resolve(&mut self, key: CaptureKey, value: Value) {
        if let Some(v) = self.inner.insert(key, Capture::Evaluated(value)) {
            if let Capture::Evaluated(_) = v {
                panic!("capture resolved more than once");
            }
        }
    }

    pub fn resolve_string_gets(&mut self, with: &HashMap<String, Value>) -> Result<()> {
        let mut resolves = vec![];
        for (k, c) in self.inner.iter_mut() {
            if let Capture::Expr(expression) = c {
                if let Some(s) = expression.value().as_ref::<ast::String>() {
                    match with.get(s.0.as_str()) {
                        Some(r) => {
                            resolves.push((*k, r.clone()));
                        }
                        None => return Err(expression.source().with("unbound value").into_error()),
                    }
                }
            }
        }
        for (k, r) in resolves {
            self.resolve(k, r);
        }
        Ok(())
    }
}

#[derive(Clone, Debug, Eq)]
struct ScopeEntryKey {
    cap: Option<CaptureKey>,
    key: EvaluatedValue,
}

impl PartialEq for ScopeEntryKey {
    fn eq(&self, other: &Self) -> bool {
        match (&self.cap, &other.cap) {
            (Some(a), Some(b)) => a == b,
            (None, None) => self.key == other.key,
            _ => false,
        }
    }
}

impl std::hash::Hash for ScopeEntryKey {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        self.cap.hash(h);
        if self.cap.is_none() {
            self.key.hash(h);
        }
    }
}

#[derive(Clone)]
struct Scope {
    inner: std::sync::Arc<RMutex<HashMap<ScopeEntryKey, Option<Value>>>>,
}

impl std::fmt::Debug for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.inner.lock())
    }
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            inner: std::sync::Arc::new(RMutex::new(Default::default())),
        }
    }

    pub fn add(&self, cap: Option<CaptureKey>, key: EvaluatedValue, value: Value) -> bool {
        self.inner
            .lock()
            .insert(ScopeEntryKey { cap, key }, Some(value))
            .map(|v| v.is_some())
            .unwrap_or(false)
    }

    pub fn init(&self, cap: Option<CaptureKey>, key: EvaluatedValue) {
        self.inner.lock().insert(ScopeEntryKey { cap, key }, None);
    }

    pub fn into_inner(self) -> Vec<(Option<CaptureKey>, EvaluatedValue, Value)> {
        match std::sync::Arc::try_unwrap(self.inner) {
            Err(a) => std::mem::take(&mut *a.lock()),
            Ok(v) => v.into_inner(),
        }
        .into_iter()
        .map(|(k, v)| (k.cap, k.key, v.unwrap_or(types::Unset.into())))
        .collect()
    }
}

#[derive(Clone)]
struct Scopes {
    inner: BTreeMap<ScopeKey, Scope>,
}

struct ScopeCloser(ScopeKey);

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            inner: Default::default(),
        }
    }

    pub fn open(&mut self, key: ScopeKey) -> ScopeCloser {
        let no_value = self.inner.insert(key, Scope::new()).is_none();
        debug_assert!(no_value, "unexpected scope present");
        ScopeCloser(key)
    }

    pub fn init(&self, scope_key: ScopeKey, cap: Option<CaptureKey>, key: EvaluatedValue) {
        self.inner
            .get(&scope_key)
            .expect("scope disappeared")
            .init(cap, key)
    }

    pub fn insert(
        &self,
        scope_key: ScopeKey,
        cap: Option<CaptureKey>,
        key: EvaluatedValue,
        value: Value,
    ) -> bool {
        self.inner
            .get(&scope_key)
            .expect("scope disappeared")
            .add(cap, key, value)
    }

    pub fn close(
        &mut self,
        closer: ScopeCloser,
    ) -> Vec<(Option<CaptureKey>, EvaluatedValue, Value)> {
        self.inner
            .remove(&closer.0)
            .expect("scope disappeared")
            .into_inner()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BlockItemMode {
    Block,
    Command,
}

impl BlockItemMode {
    fn command(&self) -> bool {
        self == &BlockItemMode::Command
    }
}

mod once {
    use std::cell::UnsafeCell;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::{
        atomic::{AtomicU8, Ordering},
        Mutex,
    };
    use std::task::{Context, Poll, Waker};

    pub struct Once<T> {
        wakers: Mutex<Vec<Waker>>,
        value: UnsafeCell<Option<T>>,
        state: AtomicU8,
    }

    unsafe impl<T: Send> Send for Once<T> {}
    unsafe impl<T: Sync> Sync for Once<T> {}

    impl<T> Default for Once<T> {
        fn default() -> Self {
            Once {
                wakers: Mutex::new(vec![]),
                value: UnsafeCell::new(None),
                state: AtomicU8::new(0),
            }
        }
    }

    const WAS_ACCESSED: u8 = 1 << 0;
    const HAS_VALUE: u8 = 1 << 1;

    #[pin_project::pin_project]
    pub struct Get<'a, T, Fut> {
        once: &'a Once<T>,
        #[pin]
        fut: Fut,
        should_poll: bool,
    }

    impl<'a, T, Fut> Future for Get<'a, T, Fut>
    where
        Fut: Future<Output = T>,
    {
        type Output = &'a T;

        fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
            let me = self.project();
            loop {
                if *me.should_poll {
                    match me.fut.poll(cx) {
                        Poll::Pending => return Poll::Pending,
                        Poll::Ready(v) => {
                            // Safety: Access protected by atomic state.
                            unsafe {
                                *me.once.value.get() = Some(v);
                            }
                            me.once.state.fetch_or(HAS_VALUE, Ordering::Release);
                            for waker in std::mem::take(&mut *me.once.wakers.lock().unwrap()) {
                                waker.wake();
                            }
                            break;
                        }
                    }
                }
                match me.once.state.fetch_or(WAS_ACCESSED, Ordering::Acquire) {
                    0 => *me.should_poll = true,
                    WAS_ACCESSED => {
                        let waker = cx.waker().clone();
                        let mut guard = me.once.wakers.lock().unwrap();
                        guard.push(waker);
                        return Poll::Pending;
                    }
                    0b11 => break,
                    _ => panic!("invalid state"),
                }
            }
            debug_assert!(me.once.state.load(Ordering::Relaxed) == (WAS_ACCESSED | HAS_VALUE));
            // Safety: atomic state guarantees that self.this.value is a valid Some(T).
            Poll::Ready(unsafe {
                (me.once.value.get() as *const Option<T>)
                    .as_ref()
                    .unwrap_unchecked()
                    .as_ref()
                    .unwrap_unchecked()
            })
        }
    }

    impl<T> Once<T> {
        pub fn get<Fut: Future<Output = T>>(&self, fut: Fut) -> Get<T, Fut> {
            Get {
                once: self,
                fut,
                should_poll: false,
            }
        }
    }
}

use once::Once;

#[derive(Clone)]
struct ExprEvaluator {
    expr: Expr,
    evaluator: Evaluator,
    captures: Captures,
    scopes: Scopes,
    scope_key: ScopeKey,
    cache: ExprEvaluatorCache,
}

#[derive(Default)]
struct ExprEvaluatorEvalCache {
    id: Once<ergo_runtime::value::Identity>,
    value: Once<Value>,
}

#[derive(Default, Clone)]
struct ExprEvaluatorCache {
    eval: std::sync::Arc<ExprEvaluatorEvalCache>,
    children: std::sync::Arc<CacheMap<usize, ExprEvaluatorCache>>,
}

fn witness<T>(_: &T) -> std::marker::PhantomData<T> {
    std::marker::PhantomData
}

impl ExprEvaluator {
    pub fn new(expr: Expr, evaluator: Evaluator, captures: Captures) -> Self {
        ExprEvaluator {
            expr,
            evaluator,
            captures,
            scopes: Scopes::new(),
            scope_key: ScopeKey::new(),
            cache: Default::default(),
        }
    }

    /// Evaluate the expression.
    ///
    /// This will evaluate the expression immediately if appropriate, otherwise it will return a
    /// dynamic value that will evaluate in the async context when needed.
    pub fn evaluate<'b>(&'b self) -> BoxFuture<'b, Value> {
        async move {
            self.cache
                .eval
                .value
                .get(self.evaluate_impl())
                .await
                .clone()
        }
        .boxed()
    }

    /// Make the evaluator a cache root, discontinuing use of the current child cache.
    fn cache_root(mut self) -> Self {
        self.cache = Default::default();
        self
    }

    fn no_eval_cache(mut self) -> Self {
        self.cache.eval = Default::default();
        self
    }

    fn child(&self, child: &Expr) -> Self {
        ExprEvaluator {
            expr: child.clone(),
            evaluator: self.evaluator,
            captures: child
                .captures()
                .map(|c| self.captures.subset(c))
                .unwrap_or_default(),
            scopes: self.scopes.clone(),
            scope_key: self.scope_key.enter(child),
            cache: self
                .cache
                .children
                .cache_default(child.instance_key())
                .clone(),
        }
    }

    fn no_scope(mut self) -> Self {
        self.scopes = Scopes::new();
        self
    }

    /// Evaluate block items.
    ///
    /// Returns the vector of normal expressions in the block and the env formed from all bindings
    /// within the block.
    async fn evaluate_block_items(
        &self,
        items: &[ast::BlockItem],
        mode: BlockItemMode,
    ) -> Result<(Vec<Value>, BTreeMap<EvaluatedValue, Value>)> {
        // Make a copy of self to modify the captures as we evaluate the block items.
        let mut me = self.clone();
        let mut env = BTreeMap::new();
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

            let close_scope = me.scopes.open(me.scope_key);

            match i {
                ast::BlockItem::Expr(e) => {
                    push_result(mode, &mut results, me.child(e).evaluate().await, last_item)
                        .await?;
                }
                ast::BlockItem::Merge(e) => {
                    let mut val = me.child(e).evaluate().await;
                    let val_source = Source::get(&val);
                    drop(Context::eval(&mut val).await);
                    match_value! { val.clone(),
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
                                    let mut key: EvaluatedValue = types::BindRestKey.into();
                                    // Safety: adding metadata does not change the identity of the
                                    // value.
                                    Source::set(unsafe { key.value_mut() }, val_source.clone());
                                    env.insert(key, val);
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
                ast::BlockItem::Bind(key, value) => {
                    let value = me.child(value).evaluate().await;
                    let key = me.child(key).evaluate().await;

                    // Immediately eval and check for bind error
                    traits::bind_no_error(key, value).await?;
                }
            }

            let new_scope = me.scopes.close(close_scope);

            for (cap, k, v) in new_scope {
                if !v.is_type::<types::Unset>() {
                    env.insert(k, v.clone());
                } else {
                    env.remove(&k);
                }
                if let Some(cap) = cap {
                    me.captures.resolve(cap, v);
                }
            }
        }

        if has_errors {
            Err(types::Error::aggregate(errs))
        } else {
            Ok((results, env))
        }
    }

    async fn evaluate_string_items(
        &self,
        items: &[ast::StringItem],
    ) -> std::result::Result<Value, ErrorOrDiagnostic> {
        let mut result = std::string::String::new();
        let mut formatter = traits::Formatter::new(&mut result);
        for item in items {
            match item {
                ast::StringItem::String(s) => formatter.write_str(s)?,
                ast::StringItem::Expression(e) => {
                    let val = self.child(e).evaluate().await;
                    traits::display(val, &mut formatter).await?;
                }
            }
        }
        drop(formatter);
        Ok(Value::from(types::String::from(result)))
    }

    fn captures_ready(&self) -> bool {
        self.expr
            .captures()
            .map(|c| self.captures.ready(c))
            .unwrap_or(true)
    }

    /// Get the identity of the expression.
    ///
    /// The identity is determined by:
    /// * traversing the AST for subexpressions/constants
    /// * calculating the identity of subexpressions (recursively), and eagerly evaluating if
    /// the captures are resolved and the identity has `should_eval` set, and
    /// * aggregating the returned identities
    ///
    /// As an optimization, we don't evaluate() all subexpressions and use eval_id(). That would be
    /// more consistent, however it would be doing a lot of extra work, and calculating identities
    /// should be doing the minimum necessary to get the identity.
    ///
    /// The identity is cached if all captures are resolved.
    fn identity<'b>(&'b self) -> BoxFuture<'b, ergo_runtime::value::Identity> {
        let captures_ready = self.captures_ready();
        let compute = async move {
            if let Some(cap) = self
                .expr
                .value()
                .as_ref::<ast::Get>()
                .map(|g| g.capture_key.expect("gets must always have a capture key"))
            {
                if let Some(v) = self.captures.get(cap) {
                    return v.clone().eval_id().await;
                }
            }

            enum IdType {
                Expr(Expr),
                Constant(u128),
            }

            let mut ids = vec![IdType::Constant(self.expr.expr_type() as u128)];

            self.expr.subexpressions(|e| {
                ids.push(match e {
                    ast::SubExpr::SubExpr(e) => IdType::Expr(e.clone()),
                    ast::SubExpr::Discriminant(u) => IdType::Constant(u as u128),
                    ast::SubExpr::Constant(v) => IdType::Constant(v),
                })
            });

            Context::global()
                .task
                .join_all(ids.into_iter().map(|t| {
                    match t {
                        IdType::Expr(e) => {
                            let c = self.child(&e);
                            async move {
                                let mut id = c.identity().await;
                                // If the child can be evaluated, eagerly evaluate to potentially
                                // end the should_eval inheritance (our identity should be based on
                                // the evaulated identity as soon as it is available).
                                if id.should_eval && c.captures_ready() {
                                    let mut v = c.evaluate().await;
                                    id = v.eval_id().await;
                                }
                                id
                            }
                            .boxed()
                        }
                        IdType::Constant(v) => {
                            let v = ergo_runtime::value::Identity::clear(v);
                            async move { v }.boxed()
                        }
                    }
                    .map(Ok)
                }))
                .await
                .unwrap()
                .into_iter()
                .sum()
        };
        if captures_ready {
            async move { self.cache.eval.as_ref().id.get(compute).await.clone() }.boxed()
        } else {
            compute.boxed()
        }
    }

    fn value_id(&self) -> ValueId {
        let me = self.clone().no_scope().no_eval_cache();
        ValueId::new(move |_| {
            Context::spawn(EVAL_TASK_PRIORITY, |_| {}, async move {
                Ok(me.identity().await)
            })
            // Error only occurs when aborting.
            .map(|l| l.unwrap_or_else(|_| ergo_runtime::value::Identity::clear(0)))
        })
    }

    fn source(&self) -> ergo_runtime::Source<()> {
        self.expr.source()
    }

    unsafe fn expr_as<T: ast::IsExpression>(&self, _: std::marker::PhantomData<T>) -> &T {
        self.expr.as_ref_unchecked::<T>()
    }

    async fn evaluate_impl(&self) -> Value {
        macro_rules! delayed {
            ( $self:ident , $v:ident , $( $body:tt )* ) => {{
                let id = self.value_id();
                let $self = self.clone().no_eval_cache();
                let v_type = witness($v);
                Value::dynamic(
                    move || async move {
                        Context::spawn(EVAL_TASK_PRIORITY, |_| {}, async move {
                            let v: Value = async move {
                                // Safety: v_type (from $v) must have been from a previous checked call
                                let $v = unsafe { $self.expr_as(v_type) };
                                $($body)*
                            }.await;
                            Ok(v)
                        })
                        .await
                        .into()
                    },
                    id,
                )
            }};
        }

        let mut val = crate::match_expression!(self.expr.value(),
            Unit(_) => types::Unit.into(),
            BindAny(_) => types::Unbound::new_no_doc(
                            |_| async { types::Unit.into() }.boxed(),
                            depends![const nsid!(expr::any)],
                        )
                        .into(),
            String(s) => types::String::from(s.0.clone()).into(),
            CompoundString(s) => delayed! { me, s,
                match me.evaluate_string_items(&s.items).await {
                    Err(ErrorOrDiagnostic::Error(e)) => return e.into(),
                    Err(ErrorOrDiagnostic::Diagnostic(mut d)) => {
                        use ergo_runtime::error::DiagnosticInfo;
                        (&mut d).add_primary_label(me.source().with("while building this string"));
                        let mut error_val = types::Error::from(d).into();
                        Source::set_if_missing(&mut error_val, me.source());
                        return error_val;
                    }
                    Ok(v) => v
                }
            },
            Array(arr) => delayed! { me, arr,
                let mut results = Vec::new();
                let mut has_errors = false;
                let mut errs = Vec::new();
                for i in &arr.items {
                    match i {
                        ast::ArrayItem::Expr(e) => {
                            results.push(me.child(e).evaluate().await);
                        }
                        ast::ArrayItem::Merge(e) => {
                            let mut val = me.child(e).evaluate().await;
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
            Block(block) => delayed! { me, block,
                match me.evaluate_block_items(&block.items, BlockItemMode::Block).await {
                    Err(e) => e.into(),
                    Ok((mut vals, env)) => {
                        debug_assert!(vals.len() < 2);
                        match vals.pop() {
                            Some(last) => last,
                            None => types::Map(env.into()).into()
                        }
                    }
                }
            },
            Function(func) => {
                let id = self.value_id();
                let bind = func.bind.clone();
                let body = func.body.clone();
                let me = self.clone().no_scope().cache_root();
                types::Unbound::new_no_doc(move |v| {
                    let bind = bind.clone();
                    let body = body.clone();
                    // We should have a separate child cache to not reuse unbound
                    // identities/values.
                    let mut me = me.clone().cache_root();
                    async move {
                        let close_scope = me.scopes.open(me.scope_key);
                        let bind = me.child(&bind).evaluate().await;
                        try_result!(traits::bind_no_error(bind, v).await);

                        let new_captures: Vec<_> = me.scopes.close(close_scope)
                            .into_iter()
                            .map(|(cap, _, v)| cap.map(|c| (c, v.clone())))
                            .collect();
                        for (cap, v) in new_captures.into_iter().filter_map(|o| o) {
                            me.captures.resolve(cap, v);
                        }

                        me.child(&body).evaluate().await
                    }.boxed()
                }, id).into()
            },
            Get(get) => {
                let cap = get.capture_key.expect("gets must always have a capture key");
                match self.captures.get(cap) {
                    Some(v) => v.clone(),
                    None => types::Unset.into(),
                }
            },
            Set(set) => {
                let me = self.clone().no_eval_cache();
                let id = self.value_id();
                let k = me.child(&set.value).evaluate().await.as_evaluated().await;
                me.scopes.init(set.scope_key, set.capture_key.clone(), k.clone());
                types::Unbound::new_no_doc(move |v| {
                    let me = me.clone();
                    let k = k.clone();
                    async move {
                        let set = unsafe { me.expr.as_ref_unchecked::<ast::Set>() };

                        if v.is_type::<types::Unset>() {
                            use ergo_runtime::error::DiagnosticInfo;
                            return ergo_runtime::diagnostic! {
                                labels: [
                                    primary(me.source().with(""))
                                ],
                                message: "cannot bind to Unset"
                            }.add_value_sources("unset", &v).into_error().into();
                        }

                        if me.scopes.insert(set.scope_key, set.capture_key.clone(), k, v.clone()) {
                            return ergo_runtime::error! {
                                labels: [
                                    primary(me.source().with("")),
                                    secondary(Source::get(&v).with("value being bound"))
                                ],
                                error: "cannot bind a setter more than once"
                            }.into();
                        }
                        types::Unit.into()
                    }.boxed()
                }, id).into()
            },
            Index(ind) => delayed! { me, ind,
                let mut value = me.child(&ind.value).evaluate().await;
                let index = me.child(&ind.index).evaluate().await;
                if let Err(e) = Context::eval(&mut value).await {
                    let mut val = e.into();
                    Source::set_if_missing(&mut val, Source::get(&value));
                    return val;
                }
                traits::bind(value, Source::imbue(me.source().with(types::Index(index).into()))).await
            },
            Command(cmd) => delayed! { me, cmd,
                let src = cmd.function.source();
                let mut function = me.child(&cmd.function).evaluate().await;
                match me
                    .evaluate_block_items(&cmd.args, BlockItemMode::Command)
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
                        traits::bind(function, Source::imbue(me.source().with(types::Args { args }.into()))).await
                    }
                }
            },
            DocComment(doc) => {
                let mut val = self.child(&doc.value).evaluate().await;
                let items = doc.items.clone();
                // FIXME this includes the identity of the value, but it should technically only
                // include the identity of the doc comment.
                // In practice the identity of a doc comment won't matter much though.
                //let id = self.value_id();
                let me = self.clone().no_scope().no_eval_cache();
                let mut doc = Value::dynamic(move || async move {
                        Context::spawn(EVAL_TASK_PRIORITY, |_| {}, async move {
                            ergo_runtime::error_info! {
                                labels: [ primary(me.source().with("while evaluating this doc comment")) ],
                                async {
                                    me.evaluate_string_items(&items).await
                                }
                            }
                        }).await.into()
                    }, 0);
                Source::set(&mut doc, self.source());
                metadata::Doc::set(&mut val, doc);
                val
            },
            Attribute(a) => delayed! { me, a,
                let attr = me.child(&a.attr).evaluate().await;
                let value = me.child(&a.value).evaluate().await;
                traits::bind(attr, value).await
            },
        );

        Source::update(&mut val, self.source());
        val
    }
}

impl Evaluator {
    /// Evaluate the given expression.
    pub async fn evaluate(self, e: Expr, captures: Captures) -> Value {
        ExprEvaluator::new(e, self, captures).evaluate().await
    }
}
