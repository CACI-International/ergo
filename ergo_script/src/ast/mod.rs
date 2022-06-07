//! The AST definition for script files.
//!
//! Expressions are erased (possibly boxed) types, to save memory use for the small, universally
//! common expressions (strings) over having an enum of all the expressions.

use ergo_runtime::abi_stable::type_erase::{Eraseable, Erased};
use ergo_runtime::source::{IntoSource, Source};
use ergo_runtime::{Error, ResultIterator};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
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
pub mod parse;
pub mod parse_tree;
pub mod tokenize;

pub type CaptureSet = keyset::KeySet;
pub type CaptureKey = keyset::Key;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ScopeKey(usize);

impl ScopeKey {
    pub const UNSET: Self = ScopeKey(usize::MAX);

    pub fn new() -> Self {
        ScopeKey(1)
    }

    pub fn enter(&self, e: &Expr) -> Self {
        let inc = match e.expr_type() {
            ExpressionType::Block | ExpressionType::Function | ExpressionType::Command => true,
            _ => false,
        };
        if inc {
            ScopeKey(self.0 + 1)
        } else {
            *self
        }
    }
}

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
}

macro_rules! expression_types {
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
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $t;
        impl Subexpressions for $t {}
        impl IsExpression for $t {
            const EXPRESSION_TYPE: ExpressionType = ExpressionType::$t;
        }
        expression_types!($($rest)*);
    };
    ( pub struct $t:ident ( $arg:ty ); $( $rest:tt )* ) => {
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $t(pub $arg);
        impl Subexpressions for $t {
            fn subexpressions<F>(&self, mut f: F)
            where
                F: FnMut(SubExpr),
            {
                let mut hasher = ergo_runtime::hash::HashFn::default();
                self.0.hash(&mut hasher);
                f(SubExpr::Constant(hasher.finish_ext()))
            }
        }
        impl IsExpression for $t {
            const EXPRESSION_TYPE: ExpressionType = ExpressionType::$t;
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            StringItem::Expression(e) => {
                f(SubExpr::Discriminant(0));
                f(SubExpr::SubExpr(e));
            }
            StringItem::String(s) => {
                f(SubExpr::Discriminant(1));
                f(SubExpr::Constant({
                    let mut hasher = ergo_runtime::hash::HashFn::default();
                    s.hash(&mut hasher);
                    hasher.finish_ext()
                }));
            }
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

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            BlockItem::Bind(key, value) => (key.source(), value.source()).into_source().with(self),
            BlockItem::Merge(e) => e.source().with(self),
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
            BlockItem::Bind(key, value) => {
                f(SubExpr::Discriminant(1));
                f(SubExpr::SubExpr(value));
                f(SubExpr::SubExpr(key));
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
            BlockItem::Bind(key, value) => {
                f(value);
                f(key);
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

pub type CommandItem = BlockItem;

expression_types! {
    pub struct Unit;

    pub struct BindAny;

    pub struct String(std::string::String);

    pub struct CompoundString { [items: StringItem] }

    pub struct Array { [items: ArrayItem] }

    pub struct Block { [items: BlockItem] }

    pub struct Function { bind, body }

    pub struct Get {
        (pub capture_key: Option<CaptureKey>),
        value
    }

    pub struct Set {
        (pub capture_key: Option<CaptureKey>),
        (pub scope_key: ScopeKey),
        value
    }

    pub struct Index { value, index }

    pub struct Command { function, [args: CommandItem] }

    pub struct DocComment {
        [items: StringItem],
        value
    }

    pub struct Attribute { attr, value }
}

/// The type of a parsed expression.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    DocComment,
    Attribute,
}

struct ExpressionInner {
    data: Erased,
    tp: ExpressionType,
}

#[derive(Clone)]
pub struct Expression {
    inner: Arc<ExpressionInner>,
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
            DocComment($e) => $body,
            Attribute($e) => $body,
        )
    };
    ( $v:expr, $t:ident, $e:pat => $body:expr ) => {
        match_expression!($v,
            Unit($e) => { type $t = Unit; $body },
            BindAny($e) => { type $t = BindAny; $body },
            String($e) => { type $t = String; $body },
            CompoundString($e) => { type $t = CompoundString; $body },
            Array($e) => { type $t = Array; $body },
            Block($e) => { type $t = Block; $body },
            Function($e) => { type $t = Function; $body },
            Get($e) => { type $t = Get; $body },
            Set($e) => { type $t = Set; $body },
            Index($e) => { type $t = Index; $body },
            Command($e) => { type $t = Command; $body },
            DocComment($e) => { type $t = DocComment; $body },
            Attribute($e) => { type $t = Attribute; $body },
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
            DocComment($e) => $body,
            Attribute($e) => $body,
        )
    };
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        if self.expr_type() != other.expr_type() {
            false
        } else {
            match_all!(self, Type, v => v == unsafe { other.as_ref_unchecked::<Type>() })
        }
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
        self.expr_type().cmp(&other.expr_type()).then_with(
            || match_all!(self, Type, v => v.cmp(unsafe { other.as_ref_unchecked::<Type>() })),
        )
    }
}

impl Hash for Expression {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.expr_type().hash(h);
        match_all!(self, v => v.hash(h));
    }
}

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
            capture_key: None,
            value,
            captures: Default::default(),
        })
    }

    pub fn set(value: Expr) -> Self {
        Self::create(Set {
            capture_key: None,
            scope_key: ScopeKey::UNSET,
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

    fn create<T: IsExpression>(t: T) -> Self {
        Expression {
            inner: Arc::new(ExpressionInner {
                tp: T::EXPRESSION_TYPE,
                data: Erased::new(t),
            }),
        }
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match_all!(self, v => write!(f, "Expression::{:?}", v))
    }
}

type Scope<K, V> = HashMap<K, V>;

struct ScopeMap<K, V> {
    scopes: Vec<Scope<K, V>>,
    // Index in `scopes`
    implicit_set_scope: ScopeKey,
}

impl<K, V> Default for ScopeMap<K, V> {
    fn default() -> Self {
        ScopeMap {
            scopes: vec![Default::default()],
            implicit_set_scope: ScopeKey::UNSET,
        }
    }
}

impl<K, V> ScopeMap<K, V> {
    pub fn down(&mut self) -> ScopeKey {
        self.scopes.push(Default::default());
        self.scope_key()
    }

    pub fn start_set_scope(&mut self) -> ScopeKey {
        let new = self.scope_key();
        std::mem::replace(&mut self.implicit_set_scope, new)
    }

    pub fn finish_set_scope(&mut self, old: ScopeKey) {
        self.implicit_set_scope = old;
    }

    pub fn up(&mut self) -> HashMap<K, V> {
        self.scopes.pop().expect("no corresponding `down`")
    }

    pub fn take(&mut self) -> HashMap<K, V> {
        std::mem::take(self.scopes.last_mut().expect("no corresponding `down`"))
    }

    pub fn scope_key(&self) -> ScopeKey {
        ScopeKey(self.scopes.len() - 1)
    }

    pub fn implicit_scope_key(&self) -> Option<ScopeKey> {
        if self.scopes.get(self.implicit_set_scope.0).is_none() {
            None
        } else {
            Some(self.implicit_set_scope)
        }
    }
}

impl<K: Eq + std::hash::Hash, V> ScopeMap<K, V> {
    pub fn insert(&mut self, k: K, v: V) -> Option<ScopeKey> {
        self.scopes.get_mut(self.implicit_set_scope.0)?.insert(k, v);
        Some(self.implicit_set_scope)
    }

    pub fn get<Q: ?Sized>(&mut self, k: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q>,
        Q: Eq + std::hash::Hash,
    {
        self.scopes.iter().rev().find_map(|i| i.get(k))
    }
}

impl Expression {
    /// Get the type of the expression.
    pub fn expr_type(&self) -> ExpressionType {
        self.inner.tp
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
            DocComment(v) => Some(&v.captures),
            Attribute(v) => Some(&v.captures),
        )
    }

    pub fn instance_key(&self) -> usize {
        Arc::as_ptr(&self.inner) as usize
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
    pub fn get_with_capture(value: Expr, key: CaptureKey) -> Self {
        Self::create(Get {
            capture_key: Some(key),
            value,
            captures: vec![key].into_iter().collect(),
        })
    }

    pub fn set_with_capture(value: Expr, key: CaptureKey, scope_key: usize) -> Self {
        Self::create(Set {
            capture_key: Some(key),
            scope_key: ScopeKey(scope_key),
            value,
            captures: Default::default(),
        })
    }

    pub fn set_with_scope_key(value: Expr, scope_key: usize) -> Self {
        Self::create(Set {
            capture_key: None,
            scope_key: ScopeKey(scope_key),
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
            DocComment(v) => v.captures = captures,
            Attribute(v) => v.captures = captures,
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
        HashMap<CaptureKey, Expr>,
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
    Ok((expr, compiler.into_free_captures()?, lint_messages))
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
    string_gets: HashMap<Expr, CaptureKey>,
    capture_mapping: ScopeMap<Expression, CaptureKey>,
    lint: Option<Lint>,
    errors: Vec<Error>,
}

impl<'a> ExpressionCompiler<'a> {
    pub fn with_context(ctx: &'a mut Context) -> Self {
        ExpressionCompiler {
            capture_context: ctx,
            string_gets: Default::default(),
            capture_mapping: Default::default(),
            lint: Default::default(),
            errors: Default::default(),
        }
    }

    pub fn enable_lint(&mut self, level: LintLevel) {
        if level == LintLevel::Off {
            self.lint = None;
        } else {
            self.lint = Some(Lint::new(level));
        }
    }

    #[allow(dead_code)]
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
                && self.capture_mapping.get(e).is_some()
            {
                v.messages.push(e.source().with(
                    "string matches a binding in scope; did you mean to use the binding?".into(),
                ));
            }
        }
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

    pub fn into_free_captures(self) -> Result<HashMap<CaptureKey, Expr>, Error> {
        if self.errors.is_empty() {
            Ok(self.string_gets.into_iter().map(|(k, v)| (v, k)).collect())
        } else {
            Err(Error::aggregate(self.errors))
        }
    }

    pub fn compile_captures(&mut self, e: &mut Expr, mut caps: &mut CaptureSet) {
        self.check_string_binding_conflict(e);
        let src = e.source();
        let e = &mut **e;

        macro_rules! do_captures {
            ( subexpr $v:expr ) => {
                do_captures!($v, |e_caps| $v
                    .subexpressions_mut(|e| self.compile_captures(e, e_caps)))
            };
            ( $v:expr, |$caps:ident| $t:stmt ) => {{
                let mut e_caps = CaptureSet::default();
                {
                    let $caps = &mut e_caps;
                    $t
                }
                caps |= &e_caps;
                $v.captures = e_caps;
            }};
        }

        match_expression_mut!(e,
            Unit(_) => (),
            BindAny(_) => (),
            String(_) => (),
            CompoundString(v) => do_captures!(subexpr v),
            Array(v) => do_captures!(subexpr v),
            Block(v) => do_captures!(v, |e_caps| {
                self.capture_mapping.down();
                // Only warn about unused sets in bindings if the last value is an expression. This
                // isn't completely accurate (a merged array may produce a final value rather than
                // a map), but it is good enough and eliminates many false-positive lints.
                let ignore_unused_bindings = v.items.last()
                        .map(|e| match e { BlockItem::Expr(_) => false, _ => true })
                        .unwrap_or_default();
                for i in &mut v.items {
                    match i {
                        BlockItem::Bind(key, value) => {
                            self.compile_captures(&mut *value, e_caps);
                            let old = self.capture_mapping.start_set_scope();
                            self.compile_captures(&mut *key, e_caps);
                            self.capture_mapping.finish_set_scope(old);
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e, e_caps),
                    }
                }
                let in_scope = self.capture_mapping.up();
                if ignore_unused_bindings {
                    in_scope.values().for_each(|k| self.used_binding(*k));
                }
                let in_scope_captures = in_scope.into_iter().map(|v| v.1).collect();

                e_caps.difference_with(&in_scope_captures);
            }),
            Function(v) => do_captures!(v, |e_caps| {
                self.capture_mapping.down();
                let old = self.capture_mapping.start_set_scope();
                self.compile_captures(&mut v.bind, e_caps);
                self.capture_mapping.finish_set_scope(old);
                self.compile_captures(&mut v.body, e_caps);
                let in_scope = self.capture_mapping.up();
                let in_scope_captures = in_scope.into_iter().map(|v| v.1).collect();

                e_caps.difference_with(&in_scope_captures);
            }),
            Get(v) => {
                // Gets may only be of a string, and will always result in a capture key.
                debug_assert!(v.value.expr_type() == ExpressionType::String);
                do_captures!(v, |e_caps| {
                    let key = self.capture_mapping.get(&v.value).cloned();
                    match key {
                        Some(key) => {
                            self.used_binding(key);
                            v.capture_key = Some(key);
                            e_caps.insert(key);
                        }
                        None => {
                            let key = *self.string_gets
                                .entry(v.value.clone())
                                .or_insert_with(|| self.capture_context.key());
                            v.capture_key = Some(key);
                            e_caps.insert(key);
                        }
                    }
                });
            },
            Set(v) => {
                if v.value.expr_type() == ExpressionType::String {
                    let key = self.capture_context.key();
                    if let Some(scope_key) = self.capture_mapping.insert(v.value.value().clone(), key) {
                        v.scope_key = scope_key;
                    } else {
                        self.errors.push(src.with("no active scope in which to bind").into_error());
                    }
                    self.unused_binding(src.with(key));
                    v.capture_key = Some(key);
                } else {
                    if let Some(scope_key) = self.capture_mapping.implicit_scope_key() {
                        v.scope_key = scope_key;
                    } else {
                        self.errors.push(src.with("no active scope in which to bind").into_error());
                    }
                    do_captures!(subexpr v);
                }
            },
            Index(v) => do_captures!(v, |e_caps| {
                self.compile_captures(&mut v.value, e_caps);
                self.ignore_string_binding_conflict().compile_captures(&mut v.index, e_caps);
            }),
            Command(v) => do_captures!(v, |e_caps| {
                self.capture_mapping.down();
                self.compile_captures(&mut v.function, e_caps);
                for i in &mut v.args {
                    match i {
                        CommandItem::Bind(key, value) => {
                            self.compile_captures(&mut *value, e_caps);
                            let old = self.capture_mapping.start_set_scope();
                            self.compile_captures(&mut *key, e_caps);
                            self.capture_mapping.finish_set_scope(old);
                            // Take the scope, as no subsequent expressions should be able to refer
                            // to anything set here.
                            let this_scope = self.capture_mapping.take();
                            // Consider any binds in scope to be used.
                            this_scope.values().for_each(|k| self.used_binding(*k));
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e, e_caps)
                    }
                }
                self.capture_mapping.up();
            }),
            DocComment(v) => do_captures!(subexpr v),
            Attribute(v) => do_captures!(subexpr v),
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

    fn top(inner: Vec<BI>) -> E {
        E::block(inner)
    }

    #[test]
    fn block_inner_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert(
            "a = 5\n$a",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("5")),
                BI::Expr(src(E::get_with_capture(s("a"), a_key))),
            ]),
        );
    }

    #[test]
    fn block_outer_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert(
            "a = 5\n{ b = 1; [$a,$b] }",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("5")),
                BI::Expr(src(E::block(vec![
                    BI::Bind(src(E::set_with_capture(s("b"), b_key, 2)), s("1")),
                    BI::Expr(src(E::array(vec![
                        AI::Expr(src(E::get_with_capture(s("a"), a_key))),
                        AI::Expr(src(E::get_with_capture(s("b"), b_key))),
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
            "a = 5\n{ b = 1; $b }",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("5")),
                BI::Expr(src(E::block(vec![
                    BI::Bind(src(E::set_with_capture(s("b"), b_key, 2)), s("1")),
                    BI::Expr(src(E::get_with_capture(s("b"), b_key))),
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
            "a = 5\n{ a = 1; $a }",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("5")),
                BI::Expr(src(E::block(vec![
                    BI::Bind(src(E::set_with_capture(s("a"), a2_key, 2)), s("1")),
                    BI::Expr(src(E::get_with_capture(s("a"), a2_key))),
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
            "a = 1\n:b -> $a",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("1")),
                BI::Expr(src(E::function(
                    src(E::set_with_capture(s("b"), b_key, 2)),
                    src(E::get_with_capture(s("a"), a_key)),
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
            "a = 1\n:b -> $b",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("1")),
                BI::Expr(src(E::function(
                    src(E::set_with_capture(s("b"), b_key, 2)),
                    src(E::get_with_capture(s("b"), b_key)),
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
            "a = 1\n:a -> $a",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("1")),
                BI::Expr(src(E::function(
                    src(E::set_with_capture(s("a"), a2_key, 2)),
                    src(E::get_with_capture(s("a"), a2_key)),
                ))),
            ]),
        );
    }

    #[test]
    fn function_pattern_and_body_overlap() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let d_key = ctx.key();
        assert_captures(
            "a:b:c :d -> a:b:e $d",
            top(vec![BI::Expr(src(E::function(
                src(E::command(
                    src(E::index(
                        src(E::index(src(E::get_with_capture(s("a"), a_key)), s("b"))
                            .set_captures(vec![a_key])),
                        s("c"),
                    )
                    .set_captures(vec![a_key])),
                    vec![CI::Expr(src(E::set_with_capture(s("d"), d_key, 2)))],
                )
                .set_captures(vec![a_key])),
                src(E::command(
                    src(E::index(
                        src(E::index(src(E::get_with_capture(s("a"), a_key)), s("b"))
                            .set_captures(vec![a_key])),
                        s("e"),
                    )
                    .set_captures(vec![a_key])),
                    vec![CI::Expr(src(E::get_with_capture(s("d"), d_key)))],
                )
                .set_captures(vec![a_key, d_key])),
            )
            .set_captures(vec![a_key])))])
            .set_captures(vec![a_key]),
            vec![(a_key, E::string("a".into()))],
        )
    }

    #[test]
    fn unbound_capture() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert_captures(
            "a b c",
            top(vec![BI::Expr(src(E::command(
                src(E::get_with_capture(s("a"), a_key)),
                vec![CI::Expr(s("b")), CI::Expr(s("c"))],
            )
            .set_captures(vec![a_key])))])
            .set_captures(vec![a_key]),
            vec![(a_key, E::string("a".into()))],
        );
    }

    #[test]
    fn multiple_function_scopes() {
        let mut ctx = keyset::Context::default();
        let fn_key = ctx.key();
        let val_key = ctx.key();
        assert_captures(
            "fn :val -> _ -> $val",
            top(vec![BI::Expr(src(E::function(
                src(E::command(
                    src(E::get_with_capture(s("fn"), fn_key)),
                    vec![CI::Expr(src(E::set_with_capture(s("val"), val_key, 2)))],
                )
                .set_captures(vec![fn_key])),
                src(E::function(
                    src(E::bind_any()),
                    src(E::get_with_capture(s("val"), val_key)),
                )
                .set_captures(vec![val_key])),
            )
            .set_captures(vec![fn_key])))])
            .set_captures(vec![fn_key]),
            vec![(fn_key, E::string("fn".into()))],
        );
    }

    #[test]
    fn multiple_unbound_captures() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        let b_key = ctx.key();
        assert_captures(
            "a $b",
            top(vec![BI::Expr(src(E::command(
                src(E::get_with_capture(s("a"), a_key)),
                vec![CI::Expr(src(E::get_with_capture(s("b"), b_key)))],
            )
            .set_captures(vec![a_key, b_key])))])
            .set_captures(vec![a_key, b_key]),
            vec![
                (a_key, E::string("a".into())),
                (b_key, E::string("b".into())),
            ],
        );
    }

    #[test]
    fn index() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert(
            "a={}\na:b:c",
            top(vec![
                BI::Bind(
                    src(E::set_with_capture(s("a"), a_key, 1)),
                    src(E::block(vec![])),
                ),
                BI::Expr(src(E::index(
                    src(E::index(src(E::get_with_capture(s("a"), a_key)), s("b"))
                        .set_captures(vec![a_key])),
                    s("c"),
                )
                .set_captures(vec![a_key]))),
            ]),
        );
    }

    #[test]
    fn dedup_common_captures() {
        let mut ctx = keyset::Context::default();
        let std_key = ctx.key();
        assert_captures(
            "std:String:format\nstd:String:from",
            top(vec![
                BI::Expr(src(E::index(
                    src(
                        E::index(src(E::get_with_capture(s("std"), std_key)), s("String"))
                            .set_captures(vec![std_key]),
                    ),
                    s("format"),
                )
                .set_captures(vec![std_key]))),
                BI::Expr(src(E::index(
                    src(
                        E::index(src(E::get_with_capture(s("std"), std_key)), s("String"))
                            .set_captures(vec![std_key]),
                    ),
                    s("from"),
                )
                .set_captures(vec![std_key]))),
            ])
            .set_captures(vec![std_key]),
            vec![(std_key, E::string("std".into()))],
        );
    }

    #[test]
    fn indirect_set() {
        let mut ctx = keyset::Context::default();
        let a_key = ctx.key();
        assert_captures(
            "a = 1; { :$a = v }",
            top(vec![
                BI::Bind(src(E::set_with_capture(s("a"), a_key, 1)), s("1")),
                BI::Expr(src(E::block(vec![BI::Bind(
                    src(
                        E::set_with_scope_key(src(E::get_with_capture(s("a"), a_key)), 2)
                            .set_captures(vec![a_key]),
                    ),
                    s("v"),
                )])
                .set_captures(vec![a_key]))),
            ]),
            vec![],
        )
    }

    #[test]
    fn nested_set() {
        let mut ctx = keyset::Context::default();
        let f_key = ctx.key();
        let a_key = ctx.key();
        assert_captures(
            "f :a = 1",
            top(vec![BI::Bind(
                src(E::command(
                    src(E::get_with_capture(s("f"), f_key)),
                    vec![BI::Expr(src(E::set_with_capture(s("a"), a_key, 1)))],
                )
                .set_captures(vec![f_key])),
                s("1"),
            )])
            .set_captures(vec![f_key]),
            vec![(f_key, E::string("f".into()))],
        )
    }

    #[test]
    fn nested_indirect_set() {
        let mut ctx = keyset::Context::default();
        let f_key = ctx.key();
        let a_key = ctx.key();
        assert_captures(
            "f :$a = 1",
            top(vec![BI::Bind(
                src(E::command(
                    src(E::get_with_capture(s("f"), f_key)),
                    vec![BI::Expr(src(E::set_with_scope_key(
                        src(E::get_with_capture(s("a"), a_key)),
                        1,
                    )
                    .set_captures(vec![a_key])))],
                )
                .set_captures(vec![f_key, a_key])),
                s("1"),
            )])
            .set_captures(vec![f_key, a_key]),
            vec![
                (a_key, E::string("a".into())),
                (f_key, E::string("f".into())),
            ],
        )
    }

    #[test]
    fn no_implicit_set_scope() {
        assert_fail(":no-scope");
    }

    mod lints {
        use super::*;

        #[test]
        fn unused_binding() {
            assert_no_lint_message("a = 100; $a");
            assert_lint_message("a = 100; b = 10; $b");
            // maps should not issue unused binding lints
            assert_no_lint_message("a = 1; b = 2");
            // commands should not issue unused binding lints
            assert_no_lint_message("a (b=1) ~c=2");
            // nested bindings should not issue unused binding lints
            assert_no_lint_message("{:a,:b} = $c");
        }

        #[test]
        fn string_binding_conflict() {
            assert_lint_message("fn :x -> x");
            assert_no_lint_message("fn :x :y -> { y, x = x:y }");
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

    fn assert_captures(s: &str, expected: E, expected_captures: Vec<(CaptureKey, E)>) {
        let (e, captures) = load(s).unwrap();
        let e = e.unwrap();
        dbg!(&e);
        dbg!(&captures);
        assert!(captures == expected_captures.into_iter().collect::<HashMap<_, _>>());
        assert!(e == expected);
    }

    fn assert_fail(s: &str) {
        load(s).unwrap_err();
    }

    fn load(s: &str) -> Result<(Expr, HashMap<CaptureKey, Expression>), Error> {
        let mut ctx = super::Context::default();
        let (e, m, _) = super::load(Source::missing(s), &mut ctx, LintLevel::Off)?;
        Ok((e, m.into_iter().map(|(k, e)| (k, e.unwrap())).collect()))
    }
}
