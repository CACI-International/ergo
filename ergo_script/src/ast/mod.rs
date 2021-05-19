//! The AST definition for script files.
//!
//! Expressions are erased (possibly boxed) types, to save memory use for the small, universally
//! common expressions (strings) over having an enum of all the expressions.

use ergo_runtime::abi_stable::type_erase::{Eraseable, Erased};
use ergo_runtime::source::{IntoSource, Source};
use ergo_runtime::{depends, Dependencies, Error, ResultIterator};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hasher;
use std::rc::Rc;
use std::sync::Arc;

pub mod keyset;
mod parse;
mod parse_tree;
mod tokenize;
mod tokenize_tree;

/// Parts of a doc comment.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DocCommentPart {
    String(std::string::String),
    ExpressionBlock(Vec<BlockItem>),
}

pub type CaptureSet = keyset::KeySet;
pub type CaptureKey = keyset::Key;

pub enum SubExpr<'a> {
    SubExpr(&'a Expression),
    Discriminant(u8),
}

pub trait Subexpressions {
    fn subexpressions<F>(&self, _f: F)
    where
        F: FnMut(SubExpr),
    {
    }

    fn subexpressions_mut<F>(&mut self, _f: F)
    where
        F: FnMut(&mut Expression),
    {
    }
}

pub trait IsExpression: Eraseable + Subexpressions {
    const EXPRESSION_TYPE: ExpressionType;

    fn hash_content<H: Hasher>(&self, h: &mut H) {
        self.subexpressions(|e| match e {
            SubExpr::Discriminant(i) => h.write_u8(i),
            SubExpr::SubExpr(e) => std::hash::Hash::hash(e, h),
        });
    }

    fn derive_captures(&mut self) {}
}

fn captures_from_expr(captures: &mut CaptureSet, expr: &Expression) {
    if let Some(Capture(key)) = expr.as_ref::<Capture>() {
        captures.insert(*key);
    } else if let Some(exprs) = expr.captures() {
        captures.union_with(exprs);
    }
}

fn auto_derive_captures<T: IsExpression>(e: &T) -> CaptureSet {
    let mut set = CaptureSet::default();
    e.subexpressions(|e| {
        if let SubExpr::SubExpr(e) = e {
            captures_from_expr(&mut set, e);
        }
    });
    set
}

macro_rules! expression_types {
    ( @imp hash [$($member:tt),*] ) => {
        fn hash_content<H: Hasher>(&self, h: &mut H) {
            $( std::hash::Hash::hash(&self.$member, h); )*
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
            F: FnMut(&mut Expression),
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

            fn derive_captures(&mut self) {
                self.captures = auto_derive_captures(self);
            }
        }
        expression_types!($($rest)*);
    };
    ( ) => { };
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
        F: FnMut(&mut Expression),
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
        F: FnMut(&mut Expression),
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
        (pub parts: Vec<DocCommentPart>),
        (pub self_capture_key: Option<CaptureKey>),
        value
    }
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

impl std::hash::Hash for Expression {
    fn hash<H: Hasher>(&self, h: &mut H) {
        h.write_u128(self.id());
    }
}

ergo_runtime::HashAsDependency!(Expression);

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

    pub fn bind_command(function: Expr, args: Vec<CommandItem>) -> Self {
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

    pub fn doc_comment(parts: Vec<DocCommentPart>, value: Expr) -> Self {
        Self::create(DocComment {
            parts,
            value,
            self_capture_key: None,
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
    ( $v:expr , $( $t:ident => |$e:pat| $body:expr , )* $( _ => $else:expr )? ) => {
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
    ( $v:expr , $( $t:ident => |$e:pat| $body:expr , )* $( _ => $else:expr )? ) => {
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
    ( $v:expr => |$e:pat| $body:expr ) => {
        match_expression!($v,
            Unit => |$e| $body,
            BindAny => |$e| $body,
            String => |$e| $body,
            Array => |$e| $body,
            Block => |$e| $body,
            Function => |$e| $body,
            Get => |$e| $body,
            Set => |$e| $body,
            Index => |$e| $body,
            Command => |$e| $body,
            PatternCommand => |$e| $body,
            Force => |$e| $body,
            Capture => |$e| $body,
            DocComment => |$e| $body,
        )
    };
    ( mut $v:expr => |$e:pat| $body:expr ) => {
        match_expression_mut!($v,
            Unit => |$e| $body,
            BindAny => |$e| $body,
            String => |$e| $body,
            Array => |$e| $body,
            Block => |$e| $body,
            Function => |$e| $body,
            Get => |$e| $body,
            Set => |$e| $body,
            Index => |$e| $body,
            Command => |$e| $body,
            PatternCommand => |$e| $body,
            Force => |$e| $body,
            Capture => |$e| $body,
            DocComment => |$e| $body,
        )
    };
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match_all!(self => |v| write!(f, "Expression({})::{:?}", &format!("{:x}", self.id())[0..8], v))
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
            Array => |v| Some(&v.captures),
            Block => |v| Some(&v.captures),
            Function => |v| Some(&v.captures),
            Get => |v| Some(&v.captures),
            Set => |v| Some(&v.captures),
            Index => |v| Some(&v.captures),
            Command => |v| Some(&v.captures),
            PatternCommand => |v| Some(&v.captures),
            Force => |v| Some(&v.captures),
            DocComment => |v| Some(&v.captures),
            _ => None
        )
    }

    fn update_capture_id(&mut self) -> bool {
        if self.expr_type() == ExpressionType::Capture {
            true
        } else {
            let mut updated = false;
            self.subexpressions_mut(|e| {
                updated |= e.update_capture_id();
            });
            if updated {
                use ergo_runtime::hash::HashFn;
                let mut hasher = HashFn::default();
                hasher.write_u8(self.expr_type() as u8);
                match_all!(self => |v| v.hash_content(&mut hasher));
                self.as_mut_inner().id = hasher.finish_ext();
                true
            } else {
                false
            }
        }
    }
}

impl Subexpressions for Expression {
    fn subexpressions<F>(&self, f: F)
    where
        F: FnMut(SubExpr),
    {
        match_all!(self => |v| v.subexpressions(f));
    }

    fn subexpressions_mut<F>(&mut self, f: F)
    where
        F: FnMut(&mut Expression),
    {
        match_all!(mut self => |v| v.subexpressions_mut(f))
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
                Unit => |v| v == unsafe { other.as_ref_unchecked::<Unit>() },
                BindAny => |v| v == unsafe { other.as_ref_unchecked::<BindAny>() },
                String => |v| v == unsafe { other.as_ref_unchecked::<String>() },
                Array => |v| v == unsafe { other.as_ref_unchecked::<Array>() },
                Block => |v| v == unsafe { other.as_ref_unchecked::<Block>() },
                Function => |v| v == unsafe { other.as_ref_unchecked::<Function>() },
                Get => |v| v == unsafe { other.as_ref_unchecked::<Get>() },
                Set => |v| v == unsafe { other.as_ref_unchecked::<Set>() },
                Index => |v| v == unsafe { other.as_ref_unchecked::<Index>() },
                Command => |v| v == unsafe { other.as_ref_unchecked::<Command>() },
                PatternCommand => |v| v == unsafe { other.as_ref_unchecked::<PatternCommand>() },
                Force => |v| v == unsafe { other.as_ref_unchecked::<Force>() },
                DocComment => |v| v == unsafe { other.as_ref_unchecked::<DocComment>() },
                Capture => |v| v == unsafe { other.as_ref_unchecked::<Capture>() },
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
            Array => |v| v.captures = captures,
            Block => |v| v.captures = captures,
            Function => |v| v.captures = captures,
            Get => |v| v.captures = captures,
            Set => |v| v.captures = captures,
            Index => |v| v.captures = captures,
            Command => |v| v.captures = captures,
            PatternCommand => |v| v.captures = captures,
            Force => |v| v.captures = captures,
            DocComment => |v| v.captures = captures,
            _ => ()
        );
        self
    }
}

/// The type of a parsed expression.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExpressionType {
    Unit,
    BindAny,
    String,
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
    Capture,
}

/// Expressions with source information.
pub type Expr = Source<Expression>;

impl From<&DocCommentPart> for Dependencies {
    fn from(part: &DocCommentPart) -> Self {
        match part {
            DocCommentPart::String(s) => depends![s],
            DocCommentPart::ExpressionBlock(es) => {
                let mut deps = depends![];
                for e in es {
                    deps += e.into()
                }
                deps
            }
        }
    }
}

impl DocCommentPart {
    /// Get the dependencies of multiple doc comment parts.
    pub fn dependencies<'a, I: IntoIterator<Item = &'a DocCommentPart>>(parts: I) -> Dependencies {
        let mut deps = depends![];
        for p in parts {
            deps += p.into();
        }
        deps
    }
}

/// Load an AST from the given character stream.
///
/// The same context must be passed among all loads of expressions that may interact with
/// eachother.
pub fn load(
    src: Source<()>,
    ctx: &mut Context,
) -> Result<(Expr, HashMap<CaptureKey, Expression>), Error> {
    let toks = tokenize::Tokens::from(src.open()?);
    let tree_toks = tokenize_tree::TreeTokens::from(toks);
    let tree_parser = parse_tree::Parser::from(tree_toks);
    let parser = parse::Parser::from(tree_parser);

    let mut expr = parser
        .map(|v| v.map_err(|e| Error::from(e)))
        .collect_result::<Vec<_>>()
        .map(|vec| {
            vec.into_source()
                .map(|v| Expression::block(v.into_iter().map(Source::unwrap).collect()))
        })?;

    let mut compiler = ExpressionCompiler::with_context(ctx);
    compiler.compile_captures(&mut *expr);
    Ok((expr, compiler.into_captures()))
}

/// Global context for compilation.
pub type Context = keyset::Context;

struct ExpressionCompiler<'a> {
    capture_context: &'a mut Context,
    captures: HashMap<Expression, CaptureKey>,
    capture_mapping: ScopeMap<u128, CaptureKey>,
}

impl<'a> ExpressionCompiler<'a> {
    pub fn with_context(ctx: &'a mut Context) -> Self {
        ExpressionCompiler {
            capture_context: ctx,
            captures: Default::default(),
            capture_mapping: Default::default(),
        }
    }

    fn capture(&mut self, e: &mut Expression) {
        // Temporarily replace the expression with a unit type until we determine
        // the capture key.
        let mut old_e = std::mem::replace(e, Expression::unit());
        old_e.update_capture_id();
        let ExpressionCompiler {
            captures,
            capture_context,
            ..
        } = self;
        let key = captures
            .entry(old_e)
            .or_insert_with(|| capture_context.key());
        *e = Expression::capture(*key);
    }

    pub fn into_captures(self) -> HashMap<CaptureKey, Expression> {
        self.captures.into_iter().map(|(k, v)| (v, k)).collect()
    }

    pub fn compile_captures(&mut self, e: &mut Expression) {
        match_expression_mut!(e,
            Unit => |v| {
                v.subexpressions_mut(|e| self.compile_captures(e));
                v.derive_captures();
            },
            BindAny => |v| {
                v.subexpressions_mut(|e| self.compile_captures(e));
                v.derive_captures();
            },
            String => |v| {
                v.subexpressions_mut(|e| self.compile_captures(e));
                v.derive_captures();
            },
            Array => |v| {
                v.subexpressions_mut(|e| self.compile_captures(e));
                v.derive_captures();
            },
            Block => |v| {
                self.capture_mapping.down();
                for i in &mut v.items {
                    match i {
                        BlockItem::Bind(b, e) => {
                            self.compile_captures(&mut *e);
                            let old_scope = self.capture_mapping.current_as_set_scope();
                            self.compile_captures(&mut *b);
                            self.capture_mapping.restore_set_scope(old_scope);
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e)
                    }
                }
                let in_scope = self.capture_mapping.up();

                v.derive_captures();
                v.captures.difference_with(&in_scope.into_iter().map(|(_,v)| v).collect());
            },
            Function => |v| {
                self.capture_mapping.down();
                let old_scope = self.capture_mapping.current_as_set_scope();
                self.compile_captures(&mut *v.bind);
                self.compile_captures(&mut *v.body);
                self.capture_mapping.restore_set_scope(old_scope);
                let in_scope = self.capture_mapping.up();

                v.derive_captures();
                v.captures.difference_with(&in_scope.into_iter().map(|(_,v)| v).collect());
            },
            Set => |v| {
                if v.value.expr_type() == ExpressionType::String {
                    let key = self.capture_context.key();
                    self.capture_mapping.insert(v.value.id(), key);
                    v.capture_key = Some(key);
                } else {
                    v.subexpressions_mut(|e| self.compile_captures(e));
                    v.derive_captures();
                }
            },
            Get => |v| {
                // Any gets of a string constant are considered forced.
                if v.value.expr_type() == ExpressionType::String {
                    // If the id is in the capture environment, use it directly
                    let key = self.capture_mapping.get(&v.value.id()).map(|v| *v);
                    match key {
                        Some(key) => *e = Expression::capture(key),
                        None => self.capture(e),
                    }
                } else {
                    v.subexpressions_mut(|e| self.compile_captures(e));
                    v.derive_captures();
                }
            },
            Index => |v| {
                // Index expressions are considered forced.
                v.subexpressions_mut(|e| self.compile_captures(e));
                v.derive_captures();
                self.capture(e);
            },
            Command => |v| {
                self.compile_captures(&mut v.function);
                for i in &mut v.args {
                    match i {
                        CommandItem::Bind(b, e) => {
                            self.compile_captures(&mut *e);
                            let old_scope = self.capture_mapping.disjoint_set_scope();
                            self.compile_captures(&mut *b);
                            self.capture_mapping.restore_set_scope(old_scope);
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e)
                    }
                }
                v.derive_captures();
            },
            PatternCommand => |v| {
                self.compile_captures(&mut v.function);
                for i in &mut v.args {
                    match i {
                        CommandItem::Bind(b, e) => {
                            self.compile_captures(&mut *e);
                            let old_scope = self.capture_mapping.disjoint_set_scope();
                            self.compile_captures(&mut *b);
                            self.capture_mapping.restore_set_scope(old_scope);
                        }
                        BlockItem::Expr(e) | BlockItem::Merge(e) => self.compile_captures(&mut *e)
                    }
                }
                v.derive_captures();
            },
            Force => |v| {
                v.subexpressions_mut(|e| self.compile_captures(e));
                v.derive_captures();
                if v.value.expr_type() != ExpressionType::Capture {
                    self.capture(&mut *v.value);
                } else {
                    // TODO: Warn about unnecessary force
                }
                // Replace this expression with the capture expression (replacing _it_ with a unit
                // placeholder; it will be dropped when `*e` is set).
                *e = std::mem::replace(&mut *v.value, Expression::unit());
            },
            Capture => |_| panic!("unexpected capture"),
            DocComment => |v| {
                v.subexpressions_mut(|e| self.compile_captures(e));

                v.captures.clear();
                captures_from_expr(&mut v.captures, &*v.value);

                self.capture_mapping.down();

                // Insert special "self" binding
                let self_id = Expression::string("self".to_owned()).id();
                let key = self.capture_context.key();
                let old_scope = self.capture_mapping.current_as_set_scope();
                self.capture_mapping.insert(self_id, key);
                self.capture_mapping.restore_set_scope(old_scope);
                v.self_capture_key = Some(key);

                for p in &mut v.parts {
                    match p {
                        DocCommentPart::ExpressionBlock(block) => {
                            for i in block {
                                match i {
                                    BlockItem::Bind(b, e) => {
                                        self.compile_captures(&mut *e);
                                        let old_scope = self.capture_mapping.current_as_set_scope();
                                        self.compile_captures(&mut *b);
                                        self.capture_mapping.restore_set_scope(old_scope);
                                        captures_from_expr(&mut v.captures, &*e);
                                        captures_from_expr(&mut v.captures, &*b);
                                    }
                                    BlockItem::Expr(e) | BlockItem::Merge(e) => {
                                        self.compile_captures(&mut *e);
                                        captures_from_expr(&mut v.captures, &*e);
                                    }
                                }
                            }
                        }
                        DocCommentPart::String(_) => ()
                    }
                }
                let in_scope = self.capture_mapping.up();

                v.captures.difference_with(&in_scope.into_iter().map(|(_,v)| v).collect());
            },
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ergo_runtime::source::{Source, StringSource};
    type E = Expression;

    type AI = ArrayItem;
    type BI = BlockItem;
    type CI = CommandItem;

    fn src<T, R>(t: T) -> R
    where
        R: From<Source<T>>,
    {
        Source::builtin(t).into()
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
            vec![(a_key, E::get(s("a")))],
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
                ),
                (
                    ind2_key,
                    E::index(src(E::capture(ind1_key)), s("c")).set_captures(vec![ind1_key]),
                ),
            ],
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
                src(E::bind_command(
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
                ),
                (
                    ind_key,
                    E::index(src(E::capture(std_key)), s("something")).set_captures(vec![std_key]),
                ),
                (std_key, E::get(s("std"))),
                (fn_key, E::get(s("fn"))),
            ],
        );
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
                (std_key, E::get(s("std"))),
                (
                    format_key,
                    E::index(src(E::capture(str_ind_key)), s("format"))
                        .set_captures(vec![str_ind_key]),
                ),
                (
                    from_key,
                    E::index(src(E::capture(str_ind_key)), s("from"))
                        .set_captures(vec![str_ind_key]),
                ),
                (
                    str_ind_key,
                    E::index(src(E::capture(std_key)), s("String")).set_captures(vec![std_key]),
                ),
            ],
        );
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

    fn load(s: &str) -> Result<(Expr, HashMap<CaptureKey, Expression>), Error> {
        let mut ctx = super::Context::default();
        super::load(
            Source::new(StringSource::new("<string>", s.to_owned())),
            &mut ctx,
        )
    }
}
