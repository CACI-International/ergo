//! The AST definition for script files.
//!
//! Expressions are erased (possibly boxed) types, to save memory use for the small, universally
//! common expressions (strings) over having an enum of all the expressions.

use ergo_runtime::abi_stable::type_erase::{Eraseable, Erased};
use ergo_runtime::source::{IntoSource, Source};
use ergo_runtime::{depends, Dependencies, Error, ResultIterator};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::sync::Arc;

trait ToDiagnostic: ToString {
    fn to_diagnostic(self) -> ergo_runtime::error::Diagnostic
    where
        Self: Sized,
    {
        let mut diag = ergo_runtime::error::Diagnostic::from("");
        self.additional_info(&mut diag);
        diag.message = self.to_string().into();
        diag
    }

    fn additional_info(&self, diagnostic: &mut ergo_runtime::error::Diagnostic);
}

pub mod keyset;
mod parse;
mod parse_tree;
mod tokenize;

pub type CaptureSet = keyset::KeySet;
pub type CaptureKey = keyset::Key;

pub enum SubExpr<'a> {
    SubExpr(&'a Expr),
    Discriminant(u8),
    Constant(u128),
}

pub trait Subexpressions {
    fn subexpressions<F>(&self, _f: F)
    where
        F: FnMut(SubExpr),
    {
    }

    fn subexpressions_mut<F>(&mut self, _f: F)
    where
        F: FnMut(&mut Expr),
    {
    }
}

pub trait IsExpression: Eraseable + Subexpressions {
    const EXPRESSION_TYPE: ExpressionType;

    fn hash_content<H: Hasher>(&self, h: &mut H) {
        self.subexpressions(|e| match e {
            SubExpr::Discriminant(i) => h.write_u8(i),
            SubExpr::SubExpr(e) => Hash::hash(e, h),
            SubExpr::Constant(v) => h.write_u128(v),
        });
    }
}

macro_rules! expression_types {
    ( @imp hash [$($member:tt),*] ) => {
        fn hash_content<H: Hasher>(&self, h: &mut H) {
            $( Hash::hash(&self.$member, h); )*
        }
    };
    ( @subexpr $self:expr , $f:expr , $name:ident ) => {
        $f(SubExpr::SubExpr(&$self.$name));
    };
    ( @subexpr mut $self:expr , $f:expr , $name:ident ) => {
        $f(&mut $self.$name);
    };
    ( @subexpr $self:expr , $f:expr , [$name:ident : $tp:ty] ) => {
        for e in $self.$name.iter() {
            e.subexpressions(|e| $f(e));
        }
        $f(SubExpr::Discriminant(0));
    };
    ( @subexpr mut $self:expr , $f:expr , [$name:ident : $tp:ty] ) => {
        for e in $self.$name.iter_mut() {
            e.subexpressions_mut(|e| $f(e));
        }
    };
    ( @subexpr $self:expr , $f:expr , [$name:ident] ) => {
        for e in $self.$name.iter() {
            $f(SubExpr::SubExpr(e));
        }
        $f(SubExpr::Discriminant(0));
    };
    ( @subexpr mut $self:expr , $f:expr , [$name:ident] ) => {
        for e in $self.$name.iter_mut() {
            $f(e);
        }
    };
    ( @subexpr $self:expr , $f:expr , {$name:ident} ) => {
        for (k,v) in $self.$name.iter() {
            $f(SubExpr::SubExpr(k));
            $f(SubExpr::SubExpr(v));
        }
        $f(SubExpr::Discriminant(0));
    };
    ( @subexpr mut $self:expr , $f:expr , {$name:ident} ) => {
        for (k,v) in $self.$name.iter_mut() {
            $f(k);
            $f(v);
        }
    };
    ( @subexpr $(mut)? $self:expr , $f:expr , ($($unused:tt)*) ) => {};
    ( @imp subexpr $($member:tt),* ) => {
        fn subexpressions<F>(&self, mut f: F)
        where
            F: FnMut(SubExpr),
        {
            $(expression_types!(@subexpr self, f, $member);)*
        }
    };
    ( @imp subexpr_mut $($member:tt),* ) => {
        fn subexpressions_mut<F>(&mut self, mut f: F)
        where
            F: FnMut(&mut Expr),
        {
            $(expression_types!(@subexpr mut self, f, $member);)*
        }
    };
    ( @make_type [$( ( $name:ident, $ty:ty ) )*] $t:ident) => {
        #[cfg_attr(test, derive(PartialEq, Eq))]
        #[derive(Debug)]
        pub struct $t {
            $( pub $name: $ty ),*,
            pub captures: CaptureSet,
        }
    };
    ( @make_type [$($toks:tt)*] $t:ident $e:ident $($members:tt)* ) => {
        expression_types!(@make_type [$($toks)* ($e, Expr)] $t $($members)*);
    };
    ( @make_type [$($toks:tt)*] $t:ident [$e:ident : $tp:ty] $($members:tt)* ) => {
        expression_types!(@make_type [$($toks)* ($e, Vec<$tp>)] $t $($members)*);
    };
    ( @make_type [$($toks:tt)*] $t:ident [$e:ident] $($members:tt)* ) => {
        expression_types!(@make_type [$($toks)* ($e, Vec<Expr>)] $t $($members)*);
    };
    ( @make_type [$($toks:tt)*] $t:ident {$e:ident} $($members:tt)* ) => {
        expression_types!(@make_type [$($toks)* ($e, std::collections::BTreeMap<Expr, Expr>)] $t $($members)*);
    };
    ( @make_type [$($toks:tt)*] $t:ident (pub $name:ident : $ty:ty) $($members:tt)* ) => {
        expression_types!(@make_type [$($toks)* ($name, $ty)] $t $($members)*);
    };
    ( pub struct $t:ident; $( $rest:tt )* ) => {
        #[cfg_attr(test, derive(PartialEq, Eq))]
        #[derive(Debug)]
        pub struct $t;
        impl Subexpressions for $t {}
        impl IsExpression for $t {
            const EXPRESSION_TYPE: ExpressionType = ExpressionType::$t;
        }
        expression_types!($($rest)*);
    };
    ( pub struct $t:ident ( $arg:ty ); $( $rest:tt )* ) => {
        #[cfg_attr(test, derive(PartialEq, Eq))]
        #[derive(Debug)]
        pub struct $t(pub $arg);
        impl Subexpressions for $t {}
        impl IsExpression for $t {
            const EXPRESSION_TYPE: ExpressionType = ExpressionType::$t;
            expression_types!(@imp hash [0]);
        }
        expression_types!($($rest)*);
    };
    ( pub struct $t:ident { $($member:tt),+ } $($rest:tt)* ) => {
        expression_types!(@make_type [] $t $($member)+);

        impl Subexpressions for $t {
            expression_types!(@imp subexpr $($member),+);
            expression_types!(@imp subexpr_mut $($member),+);
        }

        impl IsExpression for $t {
            const EXPRESSION_TYPE: ExpressionType = ExpressionType::$t;
        }
        expression_types!($($rest)*);
    };
    ( ) => { };
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum StringItem {
    String(std::string::String),
    Expression(Expr),
}

impl Subexpressions for StringItem {
    fn subexpressions<F>(&self, mut f: F)
    where
        F: FnMut(SubExpr),
    {
        match self {
            StringItem::Expression(e) => f(SubExpr::SubExpr(e)),
            StringItem::String(s) => f(SubExpr::Constant({
                let mut hasher = ergo_runtime::hash::HashFn::default();
                s.hash(&mut hasher);
                hasher.finish_ext()
            })),
        }
    }

    fn subexpressions_mut<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Expr),
    {
        match self {
            StringItem::Expression(e) => f(e),
            StringItem::String(_) => (),
        }
    }
}

impl From<&StringItem> for Dependencies {
    fn from(part: &StringItem) -> Self {
        match part {
            StringItem::String(s) => depends![s],
            StringItem::Expression(e) => depends![e],
        }
    }
}

impl StringItem {
    /// Get the dependencies of multiple string items.
    pub fn dependencies<'a, I: IntoIterator<Item = &'a StringItem>>(parts: I) -> Dependencies {
        let mut deps = depends![];
        for p in parts {
            deps += p.into();
        }
        deps
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BlockItem {
    Expr(Expr),
    Bind(Expr, Expr),
    Merge(Expr),
}

impl IntoSource for BlockItem {
    type Output = Self;

    fn into_source(self) -> Source<Self::Output> {
        match &self {
            BlockItem::Expr(e) => e.source().with(self),
            BlockItem::Bind(b, e) => (b.source(), e.source()).into_source().with(self),
            BlockItem::Merge(e) => e.source().with(self),
        }
    }
}

impl From<&BlockItem> for Dependencies {
    fn from(item: &BlockItem) -> Self {
        match item {
            BlockItem::Expr(e) | BlockItem::Merge(e) => depends![*e],
            BlockItem::Bind(b, e) => depends![*b, *e],
        }
    }
}

impl Subexpressions for BlockItem {
    fn subexpressions<F>(&self, mut f: F)
    where
        F: FnMut(SubExpr),
    {
        match self {
            BlockItem::Expr(e) => {
                f(SubExpr::Discriminant(0));
                f(SubExpr::SubExpr(e));
            }
            BlockItem::Bind(b, e) => {
                f(SubExpr::Discriminant(1));
                f(SubExpr::SubExpr(e));
                f(SubExpr::SubExpr(b));
            }
            BlockItem::Merge(e) => {
                f(SubExpr::Discriminant(2));
                f(SubExpr::SubExpr(e));
            }
        }
    }

    fn subexpressions_mut<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Expr),
    {
        match self {
            BlockItem::Expr(e) | BlockItem::Merge(e) => {
                f(e);
            }
            BlockItem::Bind(b, e) => {
                f(e);
                f(b);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ArrayItem {
    Expr(Expr),
    Merge(Expr),
}

impl Subexpressions for ArrayItem {
    fn subexpressions<F>(&self, mut f: F)
    where
        F: FnMut(SubExpr),
    {
        match self {
            ArrayItem::Expr(e) => {
                f(SubExpr::Discriminant(0));
                f(SubExpr::SubExpr(e));
            }
            ArrayItem::Merge(e) => {
                f(SubExpr::Discriminant(1));
                f(SubExpr::SubExpr(e));
            }
        }
    }

    fn subexpressions_mut<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut Expr),
    {
        match self {
            ArrayItem::Expr(e) | ArrayItem::Merge(e) => {
                f(e);
            }
        }
    }
}

impl From<&ArrayItem> for Dependencies {
    fn from(item: &ArrayItem) -> Self {
        match item {
            ArrayItem::Expr(e) | ArrayItem::Merge(e) => depends![*e],
        }
    }
}

pub type CommandItem = BlockItem;

expression_types! {
    pub struct Unit;

    pub struct BindAny;

    pub struct String(std::string::String);

    pub struct CompoundString { [items: StringItem] }

    pub struct Array { [items: ArrayItem] }

    pub struct Block { [items: BlockItem] }

    pub struct Function { bind, body }

    pub struct Get { value }

    pub struct Set {
        (pub capture_key: Option<CaptureKey>),
        value
    }

    pub struct Index { value, index }

    pub struct Command { function, [args: CommandItem] }

    pub struct PatternCommand { function, [args: CommandItem] }

    pub struct Force { value }

    pub struct Capture(CaptureKey);

    pub struct DocComment {
        [items: StringItem],
        value
    }

    pub struct Attribute { attr, value }
}

/// The type of a parsed expression.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExpressionType {
    Unit,
    BindAny,
    String,
    CompoundString,
    Array,
    Block,
    Function,
    Get,
    Set,
    Index,
    Command,
    PatternCommand,
    Force,
    DocComment,
    Attribute,
    Capture,
}

struct ExpressionInner {
    id: u128,
    data: Erased,
    tp: ExpressionType,
}

#[derive(Clone)]
pub struct Expression {
    inner: Arc<ExpressionInner>,
}

#[cfg(not(test))]
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

#[cfg(test)]
impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        self.struct_eq(other)
    }
}

impl Eq for Expression {}

impl PartialOrd for Expression {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Expression {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id().cmp(&other.id())
    }
}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, h: &mut H) {
        h.write_u128(self.id());
    }
}

ergo_runtime::ConstantDependency!(Expression);

impl Expression {
    pub fn unit() -> Self {
        Self::create(Unit)
    }

    pub fn bind_any() -> Self {
        Self::create(BindAny)
    }

    pub fn string(s: std::string::String) -> Self {
        Self::create(String(s))
    }

    pub fn compound_string(items: Vec<StringItem>) -> Self {
        Self::create(CompoundString {
            items,
            captures: Default::default(),
        })
    }

    pub fn array(items: Vec<ArrayItem>) -> Self {
        Self::create(Array {
            items,
            captures: Default::default(),
        })
    }

    pub fn block(items: Vec<BlockItem>) -> Self {
        Self::create(Block {
            items,
            captures: Default::default(),
        })
    }

    pub fn function(bind: Expr, body: Expr) -> Self {
        Self::create(Function {
            bind,
            body,
            captures: Default::default(),
        })
    }

    pub fn get(value: Expr) -> Self {
        Self::create(Get {
            value,
            captures: Default::default(),
        })
    }

    pub fn set(value: Expr) -> Self {
        Self::create(Set {
            capture_key: None,
            value,
            captures: Default::default(),
        })
    }

    pub fn index(value: Expr, index: Expr) -> Self {
        Self::create(Index {
            value,
            index,
            captures: Default::default(),
        })
    }

    pub fn command(function: Expr, args: Vec<CommandItem>) -> Self {
        Self::create(Command {
            function,
            args,
            captures: Default::default(),
        })
    }

    pub fn pat_command(function: Expr, args: Vec<CommandItem>) -> Self {
        Self::create(PatternCommand {
            function,
            args,
            captures: Default::default(),
        })
    }

    pub fn force(value: Expr) -> Self {
        Self::create(Force {
            value,
            captures: Default::default(),
        })
    }

    pub fn doc_comment(items: Vec<StringItem>, value: Expr) -> Self {
        Self::create(DocComment {
            items,
            value,
            captures: Default::default(),
        })
    }

    pub fn attribute(attr: Expr, value: Expr) -> Self {
        Self::create(Attribute {
            attr,
            value,
            captures: Default::default(),
        })
    }

    fn capture(key: CaptureKey) -> Self {
        Self::create(Capture(key))
    }

    fn create<T: IsExpression>(t: T) -> Self {
        use ergo_runtime::hash::HashFn;
        let mut hasher = HashFn::default();
        hasher.write_u8(T::EXPRESSION_TYPE as u8);
        t.hash_content(&mut hasher);
        Expression {
            inner: Arc::new(ExpressionInner {
                tp: T::EXPRESSION_TYPE,
                id: hasher.finish_ext(),
                data: Erased::new(t),
            }),
        }
    }
}

#[macro_export]
macro_rules! match_expression {
    ( $v:expr , $( $t:ident ( $e:pat ) => $body:expr , )* $( _ => $else:expr )? ) => {
        match $v.expr_type() {
            $( $crate::ast::ExpressionType::$t => {
                let $e = unsafe { $v.as_ref_unchecked::<$crate::ast::$t>() };
                $body
            } , )*
            $( _ => $else )?
        }
    }
}

macro_rules! match_expression_mut {
    ( $v:expr , $( $t:ident ( $e:pat ) => $body:expr , )* $( _ => $else:expr )? ) => {
        match $v.expr_type() {
            $( $crate::ast::ExpressionType::$t => {
                let $e = unsafe { $v.as_mut_unchecked::<$crate::ast::$t>() };
                $body
            } , )*
            $( _ => $else )?
        }
    };
}

macro_rules! match_all {
    ( $v:expr, $e:pat => $body:expr ) => {
        match_expression!($v,
            Unit($e) => $body,
            BindAny($e) => $body,
            String($e) => $body,
            CompoundString($e) => $body,
            Array($e) => $body,
            Block($e) => $body,
            Function($e) => $body,
            Get($e) => $body,
            Set($e) => $body,
            Index($e) => $body,
            Command($e) => $body,
            PatternCommand($e) => $body,
            Force($e) => $body,
            Capture($e) => $body,
            DocComment($e) => $body,
            Attribute($e) => $body,
        )
    };
    ( mut $v:expr, $e:pat => $body:expr ) => {
        match_expression_mut!($v,
            Unit($e) => $body,
            BindAny($e) => $body,
            String($e) => $body,
            CompoundString($e) => $body,
            Array($e) => $body,
            Block($e) => $body,
            Function($e) => $body,
            Get($e) => $body,
            Set($e) => $body,
            Index($e) => $body,
            Command($e) => $body,
            PatternCommand($e) => $body,
            Force($e) => $body,
            Capture($e) => $body,
            DocComment($e) => $body,
            Attribute($e) => $body,
        )
    };
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match_all!(self, v => write!(f, "Expression({})::{:?}", &format!("{:x}", self.id())[0..8], v))
    }
}

type Scope<K, V> = Rc<RefCell<HashMap<K, V>>>;

struct ScopeMap<K, V> {
    scopes: Vec<Scope<K, V>>,
    set_scope: Option<Scope<K, V>>,
}

impl<K, V> Default for ScopeMap<K, V> {
    fn default() -> Self {
        ScopeMap {
            scopes: vec![Rc::new(RefCell::new(Default::default()))],
            set_scope: None,
        }
    }
}

impl<K, V> ScopeMap<K, V> {
    pub fn down(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(Default::default())));
    }

    pub fn up(&mut self) -> HashMap<K, V> {
        match Rc::try_unwrap(self.scopes.pop().expect("no corresponding `down`")) {
            Err(_) => panic!("set scope popped"),
            Ok(v) => v.into_inner(),
        }
    }

    pub fn current_as_set_scope(&mut self) -> Option<Scope<K, V>> {
        std::mem::replace(&mut self.set_scope, self.scopes.last().map(|v| v.clone()))
    }

    pub fn disjoint_set_scope(&mut self) -> Option<Scope<K, V>> {
        std::mem::replace(
            &mut self.set_scope,
            Some(Rc::new(RefCell::new(Default::default()))),
        )
    }

    pub fn close_disjoint_set_scope(&mut self, scope: Option<Scope<K, V>>) -> HashMap<K, V> {
        match Rc::try_unwrap(std::mem::replace(&mut self.set_scope, scope).unwrap()) {
            Err(_) => panic!("invalid disjoin set scope state"),
            Ok(v) => v.into_inner(),
        }
    }

    pub fn restore_set_scope(&mut self, scope: Option<Scope<K, V>>) {
        self.set_scope = scope;
    }
}

impl<K: Eq + std::hash::Hash, V> ScopeMap<K, V> {
    pub fn insert(&mut self, k: K, v: V) {
        self.set_scope
            .as_mut()
            .expect("no set scope")
            .borrow_mut()
            .insert(k, v);
    }

    pub fn get<Q: ?Sized>(&mut self, k: &Q) -> Option<std::cell::Ref<V>>
    where
        K: std::borrow::Borrow<Q>,
        V: Clone,
        Q: Eq + std::hash::Hash,
    {
        self.scopes.iter().rev().find_map(|i| {
            let scope_ref = i.borrow();
            match scope_ref.get(k) {
                None => None,
                Some(_) => Some(std::cell::Ref::map(scope_ref, |scope| {
                    scope.get(k).unwrap()
                })),
            }
        })
    }
}

impl Expression {
    /// Get the type of the expression.
    pub fn expr_type(&self) -> ExpressionType {
        self.inner.tp
    }

    /// Get the identity of the expression.
    pub fn id(&self) -> u128 {
        self.inner.id
    }

    /// Get the expression as a reference to a particular type.
    ///
    /// If the expression is not the requested type, returns `None`.
    pub fn as_ref<T: IsExpression>(&self) -> Option<&T> {
        if T::EXPRESSION_TYPE == self.expr_type() {
            Some(unsafe { self.as_ref_unchecked::<T>() })
        } else {
            None
        }
    }

    /// Get the expression as a reference to a particular type.
    ///
    /// # Safety
    /// The expression _must_ be verified as the given type.
    pub unsafe fn as_ref_unchecked<T: IsExpression>(&self) -> &T {
        self.inner.data.as_ref::<T>()
    }

    /// Get the expression as a mutable reference to a particular type.
    ///
    /// # Safety
    /// The expression _must_ be verified as the given type.
    unsafe fn as_mut_unchecked<T: IsExpression>(&mut self) -> &mut T {
        self.as_mut_inner().data.as_mut::<T>()
    }

    fn as_mut_inner(&mut self) -> &mut ExpressionInner {
        Arc::get_mut(&mut self.inner).expect("invalid mutable access to expression")
    }

    /// Return the set of captures in this expression, if any.
    pub fn captures(&self) -> Option<&CaptureSet> {
        match_expression!(self,
            Unit(_) => None,
            BindAny(_) => None,
            String(_) => None,
            CompoundString(v) => Some(&v.captures),
            Array(v) => Some(&v.captures),
            Block(v) => Some(&v.captures),
            Function(v) => Some(&v.captures),
            Get(v) => Some(&v.captures),
            Set(v) => Some(&v.captures),
            Index(v) => Some(&v.captures),
            Command(v) => Some(&v.captures),
            PatternCommand(v) => Some(&v.captures),
            Force(v) => Some(&v.captures),
            DocComment(v) => Some(&v.captures),
            Attribute(v) => Some(&v.captures),
            Capture(_) => None,
        )
    }

    fn local_capture_id(&self) -> (u128, bool) {
        use ergo_runtime::hash::HashFn;
        let mut hasher = HashFn::default();
        hasher.write_u8(self.expr_type() as u8);
        let mut changed = false;
        self.subexpressions(|e| match e {
            SubExpr::Discriminant(i) => hasher.write_u8(i),
            SubExpr::Constant(c) => hasher.write_u128(c),
            SubExpr::SubExpr(e) => {
                let (id, c) = e.local_capture_id();
                changed |= c;
                hasher.write_u128(id);
            }
        });
        let id = if changed {
            hasher.finish_ext()
        } else {
            self.id()
        };
        changed |= self.expr_type() == ExpressionType::Capture;
        (id, changed)
    }
}

impl Subexpressions for Expression {
    fn subexpressions<F>(&self, f: F)
    where
        F: FnMut(SubExpr),
    {
        match_all!(self, v => v.subexpressions(f));
    }

    fn subexpressions_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut Expr),
    {
        match_all!(mut self, v => v.subexpressions_mut(f))
    }
}

#[cfg(test)]
impl Expression {
    pub fn id_eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }

    pub fn struct_eq(&self, other: &Self) -> bool {
        if self.expr_type() != other.expr_type() {
            false
        } else {
            match_expression!(self,
                Unit(v) => v == unsafe { other.as_ref_unchecked::<Unit>() },
                BindAny(v) => v == unsafe { other.as_ref_unchecked::<BindAny>() },
                String(v) => v == unsafe { other.as_ref_unchecked::<String>() },
                CompoundString(v) => v == unsafe { other.as_ref_unchecked::<CompoundString>() },
                Array(v) => v == unsafe { other.as_ref_unchecked::<Array>() },
                Block(v) => v == unsafe { other.as_ref_unchecked::<Block>() },
                Function(v) => v == unsafe { other.as_ref_unchecked::<Function>() },
                Get(v) => v == unsafe { other.as_ref_unchecked::<Get>() },
                Set(v) => v == unsafe { other.as_ref_unchecked::<Set>() },
                Index(v) => v == unsafe { other.as_ref_unchecked::<Index>() },
                Command(v) => v == unsafe { other.as_ref_unchecked::<Command>() },
                PatternCommand(v) => v == unsafe { other.as_ref_unchecked::<PatternCommand>() },
                Force(v) => v == unsafe { other.as_ref_unchecked::<Force>() },
                DocComment(v) => v == unsafe { other.as_ref_unchecked::<DocComment>() },
                Attribute(v) => v == unsafe { other.as_ref_unchecked::<Attribute>() },
                Capture(v) => v == unsafe { other.as_ref_unchecked::<Capture>() },
            )
        }
    }

    pub fn set_with_capture(value: Expr, key: CaptureKey) -> Self {
        Self::create(Set {
            capture_key: Some(key),
            value,
            captures: Default::default(),
        })
    }

    pub fn set_captures(mut self, keys: Vec<CaptureKey>) -> Self {
        let mut captures = CaptureSet::default();
        for k in keys {
            captures.insert(k);
        }
        let p = &mut self;
        match_expression_mut!(p,
            Unit(_) => (),
            BindAny(_) => (),
            String(_) => (),
            CompoundString(v) => v.captures = captures,
            Array(v) => v.captures = captures,
            Block(v) => v.captures = captures,
            Function(v) => v.captures = captures,
            Get(v) => v.captures = captures,
            Set(v) => v.captures = captures,
            Index(v) => v.captures = captures,
            Command(v) => v.captures = captures,
            PatternCommand(v) => v.captures = captures,
            Force(v) => v.captures = captures,
            DocComment(v) => v.captures = captures,
            Attribute(v) => v.captures = captures,
            Capture(_) => (),
        );
        self
    }
}

/// Expressions with source information.
pub type Expr = Source<Expression>;

/// Load an AST from the given string.
///
/// The same context must be passed among all loads of expressions that may interact with
/// eachother.
pub fn load(
    source: Source<&str>,
    ctx: &mut Context,
    lint: LintLevel,
) -> Result<
    (
        Expr,
        HashMap<CaptureKey, (Expr, CaptureSet)>,
        Vec<Source<std::string::String>>,
    ),
    Error,
> {
    let toks = tokenize::Tokens::from(source);
    let tree_parser = parse_tree::Parser::from(toks);
    let parser = parse::Parser::from(tree_parser);

    let mut expr = parser
        .map(|v| {
            v.map_err(|errs| {
                Error::aggregate(errs.into_iter().map(|e| {
                    use ergo_runtime::error::DiagnosticInfo;
                    let (src, e) = e.take();
                    Error::new(e.to_diagnostic().add_primary_label(src.with("")))
                }))
            })
        })
        .collect_result::<Vec<_>>()
        .map(|vec| {
            vec.into_source()
                .map(|v| Expression::block(v.into_iter().map(Source::unwrap).collect()))
        })?;

    let mut compiler = ExpressionCompiler::with_context(ctx);
    compiler.enable_lint(lint);
    compiler.compile_captures(&mut expr, &mut Default::default());
    let lint_messages = compiler.lint_messages();
    Ok((expr, compiler.into_captures(), lint_messages))
}

/// Global context for compilation.
pub type Context = keyset::Context;

#[derive(Debug, PartialEq, PartialOrd, Eq, Ord, Clone, Copy)]
/// The lint level.
pub enum LintLevel {
    Off,
    On,
    Aggressive,
}

impl std::str::FromStr for LintLevel {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "off" => Ok(LintLevel::Off),
            "on" => Ok(LintLevel::On),
            "aggressive" => Ok(LintLevel::Aggressive),
            _ => Err("invalid lint level"),
        }
    }
}

impl Default for LintLevel {
    fn default() -> Self {
        LintLevel::Off
    }
}

struct Lint {
    level: LintLevel,
    unused_bindings: HashSet<Source<CaptureKey>>,
    ignore_string_binding_conflict: bool,
    messages: Vec<Source<std::string::String>>,
}

impl Lint {
    pub fn new(level: LintLevel) -> Self {
        Lint {
            level,
            unused_bindings: Default::default(),
            ignore_string_binding_conflict: Default::default(),
            messages: Default::default(),
        }
    }
}

struct ExpressionCompiler<'a> {
    capture_context: &'a mut Context,
    captures: HashMap<u128, (CaptureKey, Expr, CaptureSet)>,
    needed_by: HashMap<CaptureKey, CaptureSet>,
    capture_mapping: ScopeMap<u128, CaptureKey>,
    lint: Option<Lint>,
}

// We have two competing goals for capture calculation to satisfy. These are:
//
// 1. We need to know which captures a particular capture depends on. This is used to determine
//    when a capture can be evaluated: when all dependent captures have been evaluated, it can be.
//    On the surface this may seem like it would be all captures that are within a captured
//    expression, but if a captured expression introduces values within subscopes, those captures
//    should not be included (because they would never be evaluated first). For this reason, having
//    _only_ the set of direct captures is not sufficient, because you may need some subset of
//    secondary captures (those that don't rely on introduced captures). For instance, in `!f (fn
//    :x -> y::x)` the outermost capture (from the forced expression) must rely on `y`, but
//    _cannot_ rely on `:x` nor `y::x`.
//
// 2. We need to be able to select the subset of captures needed for a subexpression. This is
//    essential to correctly (and efficiently) calculating the identities of subexpressions, taking
//    into account the captured (possibly evaluated) values. Importantly, if a capture _is_
//    evaluated, any sub-captures of that capture should not be included. For example, in `hello
//    (!something (!a b c))`, if `!something (!a b c)` has been evaluated to a value, the identity
//    should be derived from the literal `hello` and _only_ that value, excluding the value that
//    `!a b c` produced.
//
// The easiest way to satify (1) is to track the set of free captures (those not relying on values
// introduced within any subexpressions). To efficiently remove non-free captures when a capture
// key is determined as introduced within a scope, we use a map of backward-dependencies (i.e. we
// track which captures need each individual capture). Thus we use the map to determine which
// ancestor captures must be removed, and to calculate the map we aggregate a map of _all_ captures
// under an expression (since all of those captures will be needed by a new capture if introduced).
// This is the `all` CaptureSet. The free captures are in the `free` capture set, and are
// calculated from the union of free captures of each subexpression and then removing any
// introduced captures. The set of free captures are stored with the expression when a new capture
// is created.
//
// To satisfy (2), it is best to keep a tree structure to captures (so we can implicitly disregard
// sub-captures when a particular capture has been evaluated), so we also must track the set of
// direct captures of each _expression_ (not capture), since we must use them to determine which
// captures should be retained as we evaluate expressions. When a new capture is introduced, the
// set of direct captures is reduced to that solitary capture (disregarding any prior direct
// captures as they are now "children" of the new capture). This is the `direct` CaptureSet, which
// is stored in each expression as it has its captures compiled (for expressions that can contain
// captures). Like the `free` capture set, any captures introduced in a scope are removed from the
// `direct` CaptureSet (since you wouldn't be able to propagate a value that is introduced within
// the expression).
#[derive(Debug, Default, Clone)]
struct Captures {
    all: CaptureSet,
    direct: CaptureSet,
    free: CaptureSet,
}

impl Captures {
    pub fn insert_free(&mut self, key: CaptureKey) {
        self.all.insert(key);
        self.direct.insert(key);
        self.free.insert(key);
    }
}

impl std::ops::BitOrAssign<&'_ Self> for Captures {
    fn bitor_assign(&mut self, other: &'_ Self) {
        self.all.union_with(&other.all);
        self.direct.union_with(&other.direct);
        self.free.union_with(&other.free);
    }
}

impl std::ops::BitOrAssign<&'_ Captures> for &'_ mut Captures {
    fn bitor_assign(&mut self, other: &'_ Captures) {
        self.all.union_with(&other.all);
        self.direct.union_with(&other.direct);
        self.free.union_with(&other.free);
    }
}

impl<'a> ExpressionCompiler<'a> {
    pub fn with_context(ctx: &'a mut Context) -> Self {
        ExpressionCompiler {
            capture_context: ctx,
            captures: Default::default(),
            needed_by: Default::default(),
            capture_mapping: Default::default(),
            lint: Default::default(),
        }
    }

    pub fn enable_lint(&mut self, level: LintLevel) {
        if level == LintLevel::Off {
            self.lint = None;
        } else {
            self.lint = Some(Lint::new(level));
        }
    }

    fn add_lint(&mut self, src: Source<()>, lint: &str) {
        if let Some(v) = &mut self.lint {
            v.messages.push(src.with(lint.into()));
        }
    }

    fn unused_binding(&mut self, key: Source<CaptureKey>) {
        if let Some(v) = &mut self.lint {
            v.unused_bindings.insert(key);
        }
    }

    fn used_binding(&mut self, key: CaptureKey) {
        if let Some(v) = &mut self.lint {
            v.unused_bindings.remove(&key);
        }
    }

    fn ignore_string_binding_conflict(&mut self) -> &mut Self {
        if let Some(v) = &mut self.lint {
            v.ignore_string_binding_conflict = true;
        }
        self
    }

    fn check_string_binding_conflict(&mut self, e: &Expr) {
        if let Some(v) = &mut self.lint {
            let ignore = std::mem::take(&mut v.ignore_string_binding_conflict);
            if v.level == LintLevel::Aggressive
                && !ignore
                && e.expr_type() == ExpressionType::String
                && self.capture_mapping.get(&e.id()).is_some()
            {
                v.messages.push(e.source().with(
                    "string matches a binding in scope; did you mean to use the binding?".into(),
                ));
            }
        }
    }

    fn capture(&mut self, e: &mut Expression, source: Source<()>, e_caps: &mut Captures) {
        // Temporarily replace the expression with a unit type until we determine
        // the capture key.
        let old_e = std::mem::replace(e, Expression::unit());
        let capture_id = old_e.local_capture_id().0;
        let ExpressionCompiler {
            captures,
            capture_context,
            ..
        } = self;
        let key = captures
            .entry(capture_id)
            .or_insert_with(|| {
                (
                    capture_context.key(),
                    source.with(old_e),
                    e_caps.free.clone(),
                )
            })
            .0;
        for cap in e_caps.all.iter() {
            self.needed_by.entry(cap).or_default().insert(key);
        }
        *e = Expression::capture(key);
        e_caps.direct.clear();
        e_caps.insert_free(key);
    }

    pub fn lint_messages(&mut self) -> Vec<Source<std::string::String>> {
        self.lint
            .take()
            .map(|lint| {
                let mut messages = lint.messages;
                for v in lint.unused_bindings {
                    messages.push(v.with("unused binding".into()));
                }
                messages
            })
            .unwrap_or_default()
    }

    pub fn into_captures(self) -> HashMap<CaptureKey, (Expr, CaptureSet)> {
        self.captures
            .into_iter()
            .map(|(_, (k, e, c))| (k, (e, c)))
            .collect()
    }

    pub fn compile_captures(&mut self, e: &mut Expr, mut caps: &mut Captures) {
        self.check_string_binding_conflict(e);
        let src = e.source();
        let e = &mut **e;
        match_expression_mut!(e,
            Unit(_) => (),
            BindAny(_) => (),
            String(_) => (),
            CompoundString(v) => {
                let mut e_caps = Captures::default();
                v.subexpressions_mut(|e| self.compile_captures(e, &mut e_caps));
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            Array(v) => {
                let mut e_caps = Captures::default();
                v.subexpressions_mut(|e| self.compile_captures(e, &mut e_caps));
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            Block(v) => {
                self.capture_mapping.down();
                let mut e_caps = Captures::default();
                // Only warn about unused sets in bindings if the last value is an expression. This
                // isn't completely accurate (a merged array may produce a final value rather than
                // a map), but it is good enough and eliminates many false-positive lints.
                let ignore_unused_bindings = v.items.last()
                        .map(|e| match e { BlockItem::Expr(_) => false, _ => true })
                        .unwrap_or_default();
                for i in &mut v.items {
                    match i {
                        BlockItem::Bind(b, e) => {
                            self.compile_captures(&mut *e, &mut e_caps);
                            let old_scope = self.capture_mapping.current_as_set_scope();
                            self.compile_captures(&mut *b, &mut e_caps);
                            self.capture_mapping.restore_set_scope(old_scope);
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e, &mut e_caps)
                    }
                }
                let in_scope = self.capture_mapping.up();
                if ignore_unused_bindings {
                    in_scope.values().for_each(|k| self.used_binding(*k));
                }
                let in_scope_captures = in_scope.into_iter().map(|v| v.1).collect();

                e_caps.free.difference_with(&in_scope_captures);
                for c in in_scope_captures.iter() {
                    if let Some(set) = self.needed_by.get(&c) {
                        e_caps.free.difference_with(set);
                    }
                }

                e_caps.direct.difference_with(&in_scope_captures);
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            Function(v) => {
                self.capture_mapping.down();
                let old_scope = self.capture_mapping.current_as_set_scope();
                let mut e_caps = Captures::default();
                self.compile_captures(&mut v.bind, &mut e_caps);
                self.compile_captures(&mut v.body, &mut e_caps);
                self.capture_mapping.restore_set_scope(old_scope);
                let in_scope = self.capture_mapping.up();
                let in_scope_captures = in_scope.into_iter().map(|v| v.1).collect();

                e_caps.free.difference_with(&in_scope_captures);
                for c in in_scope_captures.iter() {
                    if let Some(set) = self.needed_by.get(&c) {
                        e_caps.free.difference_with(set);
                    }
                }

                e_caps.direct.difference_with(&in_scope_captures);
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            Set(v) => {
                if v.value.expr_type() == ExpressionType::String {
                    let key = self.capture_context.key();
                    self.capture_mapping.insert(v.value.id(), key);
                    self.unused_binding(src.with(key));
                    v.capture_key = Some(key);
                } else {
                    let mut e_caps = Captures::default();
                    v.subexpressions_mut(|e| self.compile_captures(e, &mut e_caps));
                    caps |= &e_caps;
                    v.captures = e_caps.direct;
                }
            },
            Get(v) => {
                // Any gets of a string constant are considered forced.
                if v.value.expr_type() == ExpressionType::String {
                    // If the id is in the capture environment, use it directly
                    let key = self.capture_mapping.get(&v.value.id()).map(|v| *v);
                    match key {
                        Some(key) => {
                            self.used_binding(key);
                            *e = Expression::capture(key);
                            caps.insert_free(key);
                        }
                        None => {
                            let mut e_caps = Captures::default();
                            self.capture(e, src, &mut e_caps);
                            caps |= &e_caps;
                        }
                    }
                } else {
                    let mut e_caps = Captures::default();
                    v.subexpressions_mut(|e| self.compile_captures(e, &mut e_caps));
                    caps |= &e_caps;
                    v.captures = e_caps.direct;
                }
            },
            Index(v) => {
                let mut e_caps = Captures::default();
                self.compile_captures(&mut v.value, &mut e_caps);
                self.ignore_string_binding_conflict().compile_captures(&mut v.index, &mut e_caps);
                v.captures = e_caps.direct.clone();
                // Index expressions are considered forced if the value being indexed is a capture.
                if v.value.expr_type() == ExpressionType::Capture {
                    self.capture(e, src, &mut e_caps);
                }
                caps |= &e_caps;
            },
            Command(v) => {
                let mut e_caps = Captures::default();
                self.compile_captures(&mut v.function, &mut e_caps);
                for i in &mut v.args {
                    match i {
                        CommandItem::Bind(b, e) => {
                            self.compile_captures(&mut *e, &mut e_caps);
                            let old_scope = self.capture_mapping.disjoint_set_scope();
                            self.compile_captures(&mut *b, &mut e_caps);
                            let this_scope = self.capture_mapping.close_disjoint_set_scope(old_scope);
                            // Consider any binds in scope to be used.
                            this_scope.values().for_each(|k| self.used_binding(*k));
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e, &mut e_caps)
                    }
                }
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            PatternCommand(v) => {
                let mut e_caps = Captures::default();
                self.compile_captures(&mut v.function, &mut e_caps);
                for i in &mut v.args {
                    match i {
                        CommandItem::Bind(b, e) => {
                            self.compile_captures(&mut *e, &mut e_caps);
                            let old_scope = self.capture_mapping.disjoint_set_scope();
                            self.compile_captures(&mut *b, &mut e_caps);
                            let this_scope = self.capture_mapping.close_disjoint_set_scope(old_scope);
                            // Consider any binds in scope to be used.
                            this_scope.values().for_each(|k| self.used_binding(*k));
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e, &mut e_caps)
                    }
                }
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            Force(v) => {
                let mut e_caps = Captures::default();
                self.compile_captures(&mut v.value, &mut e_caps);
                if v.value.expr_type() != ExpressionType::Capture {
                    let src = v.value.source();
                    self.capture(&mut *v.value, src, &mut e_caps);
                } else {
                    self.add_lint(src, "force expression does nothing (expression already captured)");
                }
                caps |= &e_caps;
                // Replace this expression with the capture expression (replacing _it_ with a unit
                // placeholder; it will be dropped when `*e` is set).
                *e = std::mem::replace(&mut *v.value, Expression::unit());
            },
            Capture(_) => panic!("unexpected capture"),
            DocComment(v) => {
                let mut e_caps = Captures::default();
                v.subexpressions_mut(|e| self.compile_captures(e, &mut e_caps));
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
            Attribute(v) => {
                let mut e_caps = Captures::default();
                v.subexpressions_mut(|e| self.compile_captures(e, &mut e_caps));
                caps |= &e_caps;
                v.captures = e_caps.direct;
            },
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ergo_runtime::source::Source;
    type E = Expression;

    type AI = ArrayItem;
    type BI = BlockItem;
    type CI = CommandItem;

    fn src<T, R>(t: T) -> R
    where
        R: From<Source<T>>,
    {
        Source::missing(t).into()
    }

    fn s<R>(s: &str) -> R
    where
        R: From<Source<Expression>>,
    {
        src(E::string(s.into()))
    }

    #[test]
    fn block_inner_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert(
            "a = 5\n:a",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("5")),
                BI::Expr(src(E::capture(a_key))),
            ]),
        );
    }

    #[test]
    fn block_outer_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert(
            "a = 5\n{ b = 1; [:a,:b] }",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("5")),
                BI::Expr(src(E::block(vec![
                    BI::Bind(src(E::set_with_capture(s("b"), b_key)), s("1")),
                    BI::Expr(src(E::array(vec![
                        AI::Expr(src(E::capture(a_key))),
                        AI::Expr(src(E::capture(b_key))),
                    ])
                    .set_captures(vec![a_key, b_key]))),
                ])
                .set_captures(vec![a_key]))),
            ]),
        );
    }

    #[test]
    fn block_no_outer_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert(
            "a = 5\n{ b = 1; :b }",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("5")),
                BI::Expr(src(E::block(vec![
                    BI::Bind(src(E::set_with_capture(s("b"), b_key)), s("1")),
                    BI::Expr(src(E::capture(b_key))),
                ]))),
            ]),
        );
    }

    #[test]
    fn block_shadow_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let a2_key = ctx.key();
        assert(
            "a = 5\n{ a = 1; :a }",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("5")),
                BI::Expr(src(E::block(vec![
                    BI::Bind(src(E::set_with_capture(s("a"), a2_key)), s("1")),
                    BI::Expr(src(E::capture(a2_key))),
                ]))),
            ]),
        );
    }

    #[test]
    fn function_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert(
            "a = 1\n:b -> :a",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("1")),
                BI::Expr(src(E::function(
                    src(E::set_with_capture(s("b"), b_key)),
                    src(E::capture(a_key)),
                )
                .set_captures(vec![a_key]))),
            ]),
        );
    }

    #[test]
    fn function_no_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert(
            "a = 1\n:b -> :b",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("1")),
                BI::Expr(src(E::function(
                    src(E::set_with_capture(s("b"), b_key)),
                    src(E::capture(b_key)),
                ))),
            ]),
        );
    }

    #[test]
    fn function_shadow_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let a2_key = ctx.key();
        assert(
            "a = 1\n:a -> :a",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key)), s("1")),
                BI::Expr(src(E::function(
                    src(E::set_with_capture(s("a"), a2_key)),
                    src(E::capture(a2_key)),
                ))),
            ]),
        );
    }

    #[test]
    fn function_pattern_and_body_overlap() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let ab_key = ctx.key();
        let abc_key = ctx.key();
        let d_key = ctx.key();
        let abe_key = ctx.key();
        assert_captures(
            "a:b:c :d -> a:b:e :d",
            E::block(vec![BI::Expr(src(E::function(
                src(E::pat_command(
                    src(E::capture(abc_key)),
                    vec![CI::Expr(src(E::set_with_capture(s("d"), d_key)))],
                )
                .set_captures(vec![abc_key])),
                src(E::command(
                    src(E::capture(abe_key)),
                    vec![CI::Expr(src(E::capture(d_key)))],
                )
                .set_captures(vec![abe_key, d_key])),
            )
            .set_captures(vec![abc_key, abe_key])))])
            .set_captures(vec![abc_key, abe_key]),
            vec![
                (a_key, E::get(s("a")), vec![]),
                (
                    ab_key,
                    E::index(src(E::capture(a_key)), s("b")).set_captures(vec![a_key]),
                    vec![a_key],
                ),
                (
                    abc_key,
                    E::index(src(E::capture(ab_key)), s("c")).set_captures(vec![ab_key]),
                    vec![a_key, ab_key],
                ),
                (
                    abe_key,
                    E::index(src(E::capture(ab_key)), s("e")).set_captures(vec![ab_key]),
                    vec![a_key, ab_key],
                ),
            ],
        )
    }

    #[test]
    fn unbound_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert_captures(
            "a b c",
            E::block(vec![BI::Expr(src(E::command(
                src(E::capture(a_key)),
                vec![CI::Expr(s("b")), CI::Expr(s("c"))],
            )
            .set_captures(vec![a_key])))])
            .set_captures(vec![a_key]),
            vec![(a_key, E::get(s("a")), vec![])],
        );
    }

    #[test]
    fn multiple_unbound_captures() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert_captures(
            "a :b",
            E::block(vec![BI::Expr(src(E::command(
                src(E::capture(a_key)),
                vec![CI::Expr(src(E::capture(b_key)))],
            )
            .set_captures(vec![a_key, b_key])))])
            .set_captures(vec![a_key, b_key]),
            vec![
                (a_key, E::get(s("a")), vec![]),
                (b_key, E::get(s("b")), vec![]),
            ],
        );
    }

    #[test]
    fn index() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let ind1_key = ctx.key();
        let ind2_key = ctx.key();
        assert_captures(
            "a={}\na:b:c",
            E::block(vec![
                BI::Bind(
                    src(E::set_with_capture(s("a"), a_key)),
                    src(E::block(vec![])),
                ),
                BI::Expr(src(E::capture(ind2_key))),
            ])
            .set_captures(vec![ind2_key]),
            vec![
                (
                    ind1_key,
                    E::index(src(E::capture(a_key)), s("b")).set_captures(vec![a_key]),
                    vec![a_key],
                ),
                (
                    ind2_key,
                    E::index(src(E::capture(ind1_key)), s("c")).set_captures(vec![ind1_key]),
                    vec![a_key, ind1_key],
                ),
            ],
        );
    }

    #[test]
    fn index_unforced() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert_captures(
            "a b |>:c",
            E::block(vec![BI::Expr(src(E::index(
                src(E::command(src(E::capture(a_key)), vec![CI::Expr(s("b"))])
                    .set_captures(vec![a_key])),
                s("c"),
            )
            .set_captures(vec![a_key])))])
            .set_captures(vec![a_key]),
            vec![(a_key, E::get(s("a")), vec![])],
        );
    }

    #[test]
    fn forced() {
        let mut ctx = keyset::Context::default();
        let fn_key = ctx.key();
        let a_key = ctx.key();
        let std_key = ctx.key();
        let ind_key = ctx.key();
        let force_key = ctx.key();
        assert_captures(
            "fn :a -> !std:something :a",
            E::block(vec![BI::Expr(src(E::function(
                src(E::pat_command(
                    src(E::capture(fn_key)),
                    vec![CI::Expr(src(E::set_with_capture(s("a"), a_key)))],
                )
                .set_captures(vec![fn_key])),
                src(E::capture(force_key)),
            )
            .set_captures(vec![fn_key, force_key])))])
            .set_captures(vec![fn_key, force_key]),
            vec![
                (
                    force_key,
                    E::command(
                        src(E::capture(ind_key)),
                        vec![CI::Expr(src(E::capture(a_key)))],
                    )
                    .set_captures(vec![ind_key, a_key]),
                    vec![std_key, ind_key, a_key],
                ),
                (
                    ind_key,
                    E::index(src(E::capture(std_key)), s("something")).set_captures(vec![std_key]),
                    vec![std_key],
                ),
                (std_key, E::get(s("std")), vec![]),
                (fn_key, E::get(s("fn")), vec![]),
            ],
        );
    }

    #[test]
    fn nested_forced() {
        let mut ctx = keyset::Context::default();
        let fn_key = ctx.key();
        let n_key = ctx.key();
        let a_key = ctx.key();
        let ind_key = ctx.key();
        let force_key = ctx.key();
        assert_captures(
            "!(fn :n -> a::n)",
            E::block(vec![BI::Expr(src(E::capture(force_key)))]).set_captures(vec![force_key]),
            vec![
                (fn_key, E::get(s("fn")), vec![]),
                (a_key, E::get(s("a")), vec![]),
                (
                    force_key,
                    E::function(
                        src(E::pat_command(
                            src(E::capture(fn_key)),
                            vec![CI::Expr(src(E::set_with_capture(s("n"), n_key)))],
                        )
                        .set_captures(vec![fn_key])),
                        src(E::capture(ind_key)),
                    )
                    .set_captures(vec![fn_key, ind_key]),
                    vec![fn_key, a_key],
                ),
                (
                    ind_key,
                    E::index(src(E::capture(a_key)), src(E::capture(n_key)))
                        .set_captures(vec![a_key, n_key]),
                    vec![a_key, n_key],
                ),
            ],
        )
    }

    #[test]
    fn dedup_common_captures() {
        let mut ctx = keyset::Context::default();
        let std_key = ctx.key();
        let str_ind_key = ctx.key();
        let format_key = ctx.key();
        let from_key = ctx.key();
        assert_captures(
            "std:String:format\nstd:String:from",
            E::block(vec![
                BI::Expr(src(E::capture(format_key))),
                BI::Expr(src(E::capture(from_key))),
            ])
            .set_captures(vec![format_key, from_key]),
            vec![
                (std_key, E::get(s("std")), vec![]),
                (
                    format_key,
                    E::index(src(E::capture(str_ind_key)), s("format"))
                        .set_captures(vec![str_ind_key]),
                    vec![str_ind_key, std_key],
                ),
                (
                    from_key,
                    E::index(src(E::capture(str_ind_key)), s("from"))
                        .set_captures(vec![str_ind_key]),
                    vec![str_ind_key, std_key],
                ),
                (
                    str_ind_key,
                    E::index(src(E::capture(std_key)), s("String")).set_captures(vec![std_key]),
                    vec![std_key],
                ),
            ],
        );
    }

    // TODO determine what sort of behavior would be most appropriate for this case
    #[test]
    #[should_panic]
    fn set_scope_in_pattern_command() {
        let mut ctx = keyset::Context::default();
        let a1 = ctx.key();
        let f = ctx.key();
        let a2 = ctx.key();
        assert_captures(
            "a = 1; f :a !(a ()) = ()",
            E::block(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a1)), s("1")),
                BI::Bind(
                    src(E::pat_command(
                        src(E::capture(f)),
                        vec![
                            CI::Expr(src(E::set_with_capture(s("a"), a2))),
                            CI::Expr(src(E::command(
                                src(E::capture(a1)),
                                vec![CI::Expr(src(E::unit()))],
                            )
                            .set_captures(vec![a1]))),
                        ],
                    )
                    .set_captures(vec![f, a1])),
                    src(E::unit()),
                ),
            ])
            .set_captures(vec![f]),
            vec![(f, E::get(s("f")), vec![])],
        );
    }

    mod lints {
        use super::*;

        #[test]
        fn unused_binding() {
            assert_no_lint_message("a = 100; :a");
            assert_lint_message("a = 100; b = 10; :b");
            // maps should not issue unused binding lints
            assert_no_lint_message("a = 1; b = 2");
            // commands should not issue unused binding lints
            assert_no_lint_message("a (b=1)");
            // nested bindings should not issue unused binding lints
            assert_no_lint_message("{a,b} = :c");
        }

        #[test]
        fn string_binding_conflict() {
            assert_lint_message("fn :x -> x");
            assert_no_lint_message("fn :x :y -> { y, x = x:y }");
        }

        #[test]
        fn unnecessary_force() {
            assert_lint_message("a = {}, !a:b");
            assert_lint_message("!:something");
        }

        fn assert_lint_message(s: &str) {
            let mut ctx = super::super::Context::default();
            let (_, _, lints) =
                super::super::load(Source::missing(s), &mut ctx, LintLevel::Aggressive).unwrap();
            dbg!(&lints);
            assert!(!lints.is_empty());
        }

        fn assert_no_lint_message(s: &str) {
            let mut ctx = super::super::Context::default();
            let (_, _, lints) =
                super::super::load(Source::missing(s), &mut ctx, LintLevel::Aggressive).unwrap();
            dbg!(&lints);
            assert!(lints.is_empty());
        }
    }

    fn assert(s: &str, expected: E) {
        assert_captures(s, expected, vec![]);
    }

    fn assert_captures(
        s: &str,
        expected: E,
        expected_captures: Vec<(CaptureKey, E, Vec<CaptureKey>)>,
    ) {
        let (e, captures) = load(s).unwrap();
        let e = e.unwrap();
        dbg!(&e);
        dbg!(&captures);
        assert!(
            captures
                == expected_captures
                    .into_iter()
                    .map(|(key, e, free)| (key, (e, free.into_iter().collect())))
                    .collect::<HashMap<_, _>>()
        );
        assert!(e.struct_eq(&expected));
    }

    fn load(s: &str) -> Result<(Expr, HashMap<CaptureKey, (Expression, CaptureSet)>), Error> {
        let mut ctx = super::Context::default();
        let (e, m, _) = super::load(Source::missing(s), &mut ctx, LintLevel::Off)?;
        Ok((
            e,
            m.into_iter()
                .map(|(k, (e, s))| (k, (e.unwrap(), s)))
                .collect(),
        ))
    }
}
