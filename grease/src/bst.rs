//! An ABI-stable binary search tree.

use abi_stable::{
    std_types::{RBox, ROption},
    StableAbi,
};
use pin_project::pin_project;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::pin::Pin;

use ROption::*;

pub use map::BstMap;
pub use set::BstSet;

type BoxedNode<K, V> = Pin<RBox<Node<K, V>>>;

#[pin_project]
#[derive(Debug, StableAbi)]
#[repr(C)]
struct Node<K, V> {
    key: K,
    value: V,
    #[pin]
    parent: *const Node<K, V>,
    #[pin]
    left: ROption<BoxedNode<K, V>>,
    #[pin]
    right: ROption<BoxedNode<K, V>>,
    #[pin]
    _pin: std::marker::PhantomPinned,
}

// By default, Node<K, V> is not Send/Sync because of the raw pointer.
unsafe impl<K: Send, V: Send> Send for Node<K, V> {}
unsafe impl<K: Sync, V: Sync> Sync for Node<K, V> {}

/// Convert an RBox node into a Box node.
/// This is necessary as RBox::into_box may move the value.
fn into_box<K, V>(node: BoxedNode<K, V>) -> Pin<Box<Node<K, V>>> {
    let prev = &*node as *const _;
    let mut b = unsafe { RBox::into_box(Pin::into_inner_unchecked(node)) };
    let parent = &*b as *const _;
    if prev != parent {
        if let RSome(l) = &mut b.left {
            *l.as_mut().project().parent = parent;
        }
        if let RSome(r) = &mut b.right {
            *r.as_mut().project().parent = parent;
        }
    }
    unsafe { Pin::new_unchecked(b) }
}

#[allow(dead_code)]
impl<K, V> Node<K, V> {
    pub fn new(parent: *const Self, key: K, value: V) -> BoxedNode<K, V> {
        RBox::pin(Node {
            key,
            value,
            parent,
            left: RNone,
            right: RNone,
            _pin: std::marker::PhantomPinned,
        })
    }

    pub fn first(&self) -> &Self {
        match &self.left {
            RNone => self,
            RSome(v) => v.first(),
        }
    }

    /// Unsafe because the returned node must retake ownership of parent(s).
    pub unsafe fn first_owned(self: Pin<Box<Self>>) -> Pin<Box<Self>> {
        if self.left.is_none() {
            self
        } else {
            let mut b = Pin::into_inner_unchecked(self);
            let l = b.left.take().unwrap();
            // Release box ownership; it is up to the child nodes to reassume ownership of the
            // parent.
            debug_assert!(l.parent == (&*b) as *const _);
            std::mem::forget(b);
            into_box(l).first_owned()
        }
    }

    /// Unsafe because self must no longer be used in a tree.
    pub unsafe fn claim_parent(&self) -> Pin<Box<Self>> {
        debug_assert!(self.left.is_none());
        debug_assert!(self.right.is_none());
        Pin::new_unchecked(Box::from_raw(self.parent as *mut _))
    }

    pub fn next_parent(&self) -> Option<&Self> {
        unsafe { self.parent.as_ref() }.and_then(|parent| match &parent.left {
            RNone => parent.next_parent(),
            RSome(l) => {
                if std::ptr::eq(self, &**l) {
                    Some(parent)
                } else {
                    parent.next_parent()
                }
            }
        })
    }

    /// Unsafe because caller must ensure no references to the node exist in the children.
    pub unsafe fn into_value(this: BoxedNode<K, V>) -> V {
        debug_assert!(this.left.is_none());
        debug_assert!(this.right.is_none());
        RBox::into_inner(Pin::into_inner_unchecked(this)).value
    }
}

fn clone_node<K: Clone, V: Clone>(
    v: &BoxedNode<K, V>,
    parent: *const Node<K, V>,
) -> BoxedNode<K, V> {
    let mut n = Node::new(parent, v.key.clone(), v.value.clone());
    let ptr: *const Node<K, V> = &*n;
    let mut p = n.as_mut().project();
    *p.left = match v.left {
        RNone => RNone,
        RSome(ref v) => RSome(clone_node(v, ptr)),
    };
    *p.right = match v.right {
        RNone => RNone,
        RSome(ref v) => RSome(clone_node(v, ptr)),
    };
    n
}

fn find_node<'a, Q, K, V>(
    mut node: &'a ROption<BoxedNode<K, V>>,
    key: &Q,
) -> (*const Node<K, V>, &'a ROption<BoxedNode<K, V>>)
where
    K: std::borrow::Borrow<Q>,
    Q: Ord + ?Sized,
{
    let mut parent: *const Node<K, V> = std::ptr::null();
    while let RSome(n) = node {
        match key.cmp(n.key.borrow()) {
            Ordering::Less => {
                parent = &**n;
                node = &n.left;
            }
            Ordering::Greater => {
                parent = &**n;
                node = &n.right;
            }
            Ordering::Equal => {
                break;
            }
        }
    }
    (parent, node)
}

fn find_node_mut<'a, Q, K, V>(
    node: &'a mut ROption<BoxedNode<K, V>>,
    key: &Q,
) -> (*mut Node<K, V>, &'a mut ROption<BoxedNode<K, V>>)
where
    K: std::borrow::Borrow<Q>,
    Q: Ord + ?Sized,
{
    unsafe { std::mem::transmute(find_node(node, key)) }
}

mod map {
    use super::*;
    use abi_stable::{std_types::ROption, StableAbi};

    #[derive(Debug, StableAbi)]
    #[repr(C)]
    pub struct BstMap<K, V> {
        root: ROption<BoxedNode<K, V>>,
        len: usize,
    }

    #[allow(dead_code)]
    impl<K, V> BstMap<K, V> {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn is_empty(&self) -> bool {
            self.len == 0
        }

        pub fn len(&self) -> usize {
            self.len
        }

        pub fn clear(&mut self) {
            self.root = RNone;
            self.len = 0;
        }

        pub fn iter(&self) -> Iter<K, V> {
            Iter {
                node: self.root.as_ref().map(|v| v.first()).into_option(),
                len: self.len,
            }
        }

        pub fn iter_mut(&mut self) -> IterMut<K, V> {
            IterMut { inner: self.iter() }
        }

        pub fn append(&mut self, other: &mut Self)
        where
            K: Ord,
        {
            for (k, v) in std::mem::take(other) {
                self.insert(k, v);
            }
        }

        pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
        where
            K: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            let (parent, node) = find_node_mut(&mut self.root, key);
            match node.take() {
                RNone => None,
                RSome(mut val) => {
                    let mut proj = val.as_mut().project();
                    self.len -= 1;
                    match (proj.left.take(), proj.right.take()) {
                        (RNone, RNone) => (),
                        (RSome(mut l), RNone) => {
                            *l.as_mut().project().parent = parent;
                            *node = RSome(l);
                        }
                        (RNone, RSome(mut r)) => {
                            *r.as_mut().project().parent = parent;
                            *node = RSome(r);
                        }
                        (RSome(l), RSome(r)) => {
                            let (mut root, mut other) = if (&*l as *const _) < (&*r as *const _) {
                                (l, r)
                            } else {
                                (r, l)
                            };
                            *root.as_mut().project().parent = parent;
                            *node = RSome(root);
                            let (parent, ins) = find_node_mut(&mut self.root, val.key.borrow());
                            debug_assert!(ins.is_none());
                            *other.as_mut().project().parent = parent;
                            *ins = RSome(other);
                        }
                    }
                    Some(unsafe { Node::into_value(val) })
                }
            }
        }

        pub fn insert(&mut self, key: K, mut value: V) -> Option<V>
        where
            K: Ord,
        {
            let (parent, node) = find_node_mut(&mut self.root, &key);
            match node {
                RNone => {
                    *node = RSome(Node::new(parent, key, value));
                    self.len += 1;
                    None
                }
                RSome(val) => {
                    std::mem::swap(val.as_mut().project().value, &mut value);
                    Some(value)
                }
            }
        }

        pub fn get<Q>(&self, key: &Q) -> Option<&V>
        where
            K: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            find_node(&self.root, key)
                .1
                .as_ref()
                .map(|v| &v.value)
                .into_option()
        }

        pub fn get_key_value<Q>(&self, key: &Q) -> Option<(&K, &V)>
        where
            K: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            find_node(&self.root, key)
                .1
                .as_ref()
                .map(|v| (&v.key, &v.value))
                .into_option()
        }

        pub fn contains_key<Q>(&self, key: &Q) -> bool
        where
            K: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            find_node(&self.root, key).1.is_some()
        }
    }

    impl<K, V> Default for BstMap<K, V> {
        fn default() -> Self {
            BstMap {
                root: RNone,
                len: 0,
            }
        }
    }

    impl<K: Clone, V: Clone> Clone for BstMap<K, V> {
        fn clone(&self) -> Self {
            BstMap {
                root: match &self.root {
                    RNone => RNone,
                    RSome(v) => RSome(clone_node(&v, std::ptr::null())),
                },
                len: self.len,
            }
        }
    }

    impl<K: std::cmp::Ord, V> From<BTreeMap<K, V>> for BstMap<K, V> {
        fn from(m: BTreeMap<K, V>) -> Self {
            m.into_iter().collect()
        }
    }

    impl<K: std::cmp::Ord, V> From<BstMap<K, V>> for BTreeMap<K, V> {
        fn from(m: BstMap<K, V>) -> Self {
            m.into_iter().collect()
        }
    }

    impl<K: std::hash::Hash, V: std::hash::Hash> std::hash::Hash for BstMap<K, V> {
        fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
            for i in self.iter() {
                i.hash(h);
            }
        }
    }

    impl<K: Ord, V> Extend<(K, V)> for BstMap<K, V> {
        fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
            for (k, v) in iter {
                self.insert(k, v);
            }
        }
    }

    impl<K, V> IntoIterator for BstMap<K, V> {
        type IntoIter = IntoIter<K, V>;
        type Item = (K, V);

        fn into_iter(self) -> Self::IntoIter {
            IntoIter {
                node: self
                    .root
                    .map(|v| unsafe { into_box(v).first_owned() })
                    .into_option(),
                len: self.len,
            }
        }
    }

    impl<'a, K, V> IntoIterator for &'a BstMap<K, V> {
        type IntoIter = Iter<'a, K, V>;
        type Item = (&'a K, &'a V);

        fn into_iter(self) -> Self::IntoIter {
            self.iter()
        }
    }

    impl<K, V> std::iter::FromIterator<(K, V)> for BstMap<K, V>
    where
        K: Ord,
    {
        fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
            let mut ret = Self::default();
            for (k, v) in iter {
                ret.insert(k, v);
            }
            ret
        }
    }

    impl<K: PartialEq, V: PartialEq> PartialEq for BstMap<K, V> {
        fn eq(&self, other: &Self) -> bool {
            if self.len() != other.len() {
                return false;
            }

            let mut a = self.iter();
            let mut b = other.iter();
            while let (Some(va), Some(vb)) = (a.next(), b.next()) {
                if va != vb {
                    return false;
                }
            }
            true
        }
    }

    impl<K: Eq, V: Eq> Eq for BstMap<K, V> {}

    #[derive(Debug)]
    pub struct Iter<'a, K, V>
    where
        K: 'a,
        V: 'a,
    {
        node: Option<&'a Node<K, V>>,
        len: usize,
    }

    #[derive(Debug)]
    pub struct IterMut<'a, K, V>
    where
        K: 'a,
        V: 'a,
    {
        inner: Iter<'a, K, V>,
    }

    #[derive(Debug)]
    pub struct IntoIter<K, V> {
        node: Option<Pin<Box<Node<K, V>>>>,
        len: usize,
    }

    impl<'a, K, V> Iterator for Iter<'a, K, V> {
        type Item = (&'a K, &'a V);

        fn next(&mut self) -> Option<Self::Item> {
            match self.node {
                None => None,
                Some(n) => {
                    debug_assert!(self.len > 0);
                    self.len -= 1;
                    match &n.right {
                        RSome(r) => {
                            self.node = Some(r.first());
                        }
                        RNone => {
                            self.node = n.next_parent();
                        }
                    }
                    Some((&n.key, &n.value))
                }
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.len, Some(self.len))
        }
    }

    impl<'a, K, V> ExactSizeIterator for Iter<'a, K, V> {
        fn len(&self) -> usize {
            self.len
        }
    }

    impl<'a, K, V> std::iter::FusedIterator for Iter<'a, K, V> {}

    impl<'a, K, V> Iterator for IterMut<'a, K, V> {
        type Item = (&'a mut K, &'a mut V);

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|v| unsafe { std::mem::transmute(v) })
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
    }

    impl<'a, K, V> ExactSizeIterator for IterMut<'a, K, V> {
        fn len(&self) -> usize {
            self.inner.len()
        }
    }

    impl<'a, K, V> std::iter::FusedIterator for IterMut<'a, K, V> {}

    impl<K, V> Iterator for IntoIter<K, V> {
        type Item = (K, V);

        fn next(&mut self) -> Option<Self::Item> {
            match self.node.take() {
                None => None,
                Some(n) => {
                    self.len -= 1;
                    let mut n = unsafe { Pin::into_inner_unchecked(n) };
                    if n.right.is_none() {
                        self.node = if n.parent.is_null() {
                            None
                        } else {
                            Some(unsafe { n.claim_parent() })
                        };
                    } else {
                        let mut r = n.right.take().unwrap();
                        *r.as_mut().project().parent = n.parent;
                        self.node = Some(unsafe { into_box(r).first_owned() });
                    }
                    Some((n.key, n.value))
                }
            }
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            (self.len, Some(self.len))
        }
    }

    impl<K, V> ExactSizeIterator for IntoIter<K, V> {
        fn len(&self) -> usize {
            self.len
        }
    }

    impl<K, V> std::iter::FusedIterator for IntoIter<K, V> {}

    impl<K, V> Drop for IntoIter<K, V> {
        fn drop(&mut self) {
            while self.next().is_some() {}
        }
    }

    #[cfg(test)]
    mod test {
        use super::BstMap;

        #[test]
        fn default() {
            let m: BstMap<u8, u8> = BstMap::default();
            assert_eq!(m.len(), 0);
            assert!(m.is_empty());
        }

        #[test]
        fn insert() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.insert(2, 2), None);
            assert_eq!(m.insert(3, 3), None);
            assert_eq!(m.insert(8, 4), None);
            assert_eq!(m.insert(1, 5), None);
            assert_eq!(m.len(), 5);
            assert!(!m.is_empty());
            assert_eq!(m.insert(2, 6), Some(2));
            assert_eq!(m.insert(8, 9), Some(4));
            assert_eq!(m.len(), 5);
        }

        #[test]
        fn get() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.insert(2, 2), None);
            assert_eq!(m.get(&1), None);
            assert_eq!(m.get(&2), Some(&2));
            assert_eq!(m.get(&5), Some(&1));
        }

        #[test]
        fn get_key_value() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.get_key_value(&2), None);
            assert_eq!(m.get_key_value(&5), Some((&5, &1)));
        }

        #[test]
        fn contains_key() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.contains_key(&2), false);
            assert_eq!(m.contains_key(&5), true);
        }

        #[test]
        fn iter() {
            {
                let mut m: BstMap<u8, u8> = BstMap::new();
                assert_eq!(m.insert(5, 1), None);
                assert_eq!(m.insert(2, 2), None);
                assert_eq!(m.insert(3, 3), None);
                assert_eq!(m.insert(8, 4), None);
                assert_eq!(m.insert(1, 5), None);
                assert_eq!(m.len(), 5);
                assert!(!m.is_empty());
                assert_eq!(m.insert(2, 6), Some(2));

                let entries: Vec<_> = m.iter().collect();
                assert_eq!(entries.len(), m.len());
                let expected: Vec<(&u8, &u8)> =
                    vec![(&1, &5), (&2, &6), (&3, &3), (&5, &1), (&8, &4)];
                assert_eq!(entries, expected);
            }
            {
                let mut m: BstMap<String, u8> = BstMap::new();
                m.insert("alpha".into(), 1);
                m.insert("beta".into(), 2);
                let mut i = m.iter();
                assert_eq!(i.next(), Some((&"alpha".to_owned(), &1)));
                assert_eq!(i.next(), Some((&"beta".to_owned(), &2)));
                assert_eq!(i.next(), None);
            }
        }

        #[test]
        fn into_iter() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.insert(2, 2), None);
            assert_eq!(m.insert(3, 3), None);
            assert_eq!(m.insert(8, 4), None);
            assert_eq!(m.insert(1, 5), None);
            assert_eq!(m.len(), 5);
            assert!(!m.is_empty());
            assert_eq!(m.insert(2, 6), Some(2));

            let entries: Vec<_> = m.into_iter().collect();
            assert_eq!(entries.len(), 5);
            let expected: Vec<(u8, u8)> = vec![(1, 5), (2, 6), (3, 3), (5, 1), (8, 4)];
            assert_eq!(entries, expected);
        }

        #[test]
        fn from_iter() {
            use std::iter::FromIterator;
            let entries = vec![(5, 1), (4, 3), (1, 2), (2, 2), (3, 2), (4, 2)];
            let m = <BstMap<u8, u8>>::from_iter(entries);
            assert_eq!(
                m.into_iter().collect::<Vec<_>>(),
                vec![(1, 2), (2, 2), (3, 2), (4, 2), (5, 1)]
            );
        }

        #[test]
        fn clone() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.insert(2, 2), None);
            assert_eq!(m.insert(3, 3), None);
            assert_eq!(m.insert(8, 4), None);
            assert_eq!(m.insert(1, 5), None);
            let cloned = m.clone();
            assert_eq!(m.len(), cloned.len());
            assert_eq!(m, cloned);
            let m: Vec<_> = m.into_iter().collect();
            let cloned: Vec<_> = cloned.into_iter().collect();
            assert_eq!(m, cloned);
        }

        #[test]
        fn remove() {
            let mut m: BstMap<u8, u8> = BstMap::new();
            assert_eq!(m.insert(5, 1), None);
            assert_eq!(m.insert(2, 2), None);
            assert_eq!(m.insert(3, 3), None);
            assert_eq!(m.insert(8, 4), None);
            assert_eq!(m.insert(1, 5), None);
            assert_eq!(m.len(), 5);
            assert!(!m.is_empty());
            assert_eq!(m.insert(2, 6), Some(2));

            assert_eq!(m.remove(&6), None);
            assert_eq!(m.remove(&3), Some(3));
            assert_eq!(m.len(), 4);
            assert_eq!(m.remove(&3), None);
            assert_eq!(m.remove(&1), Some(5));
            assert_eq!(m.len(), 3);
            m.clear();
            assert!(m.is_empty());
        }
    }
}

mod set {
    use super::map::*;
    use abi_stable::StableAbi;
    use std::collections::BTreeSet;

    #[derive(Clone, Debug, Hash, StableAbi)]
    #[repr(C)]
    pub struct BstSet<T> {
        inner: BstMap<T, ()>,
    }

    #[allow(dead_code)]
    impl<T> BstSet<T> {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn is_empty(&self) -> bool {
            self.inner.is_empty()
        }

        pub fn len(&self) -> usize {
            self.inner.len()
        }

        pub fn clear(&mut self) {
            self.inner.clear()
        }

        pub fn iter(&self) -> Iter<T> {
            Iter {
                inner: self.inner.iter(),
            }
        }

        pub fn iter_mut(&mut self) -> IterMut<T> {
            IterMut {
                inner: self.inner.iter_mut(),
            }
        }

        pub fn remove<Q>(&mut self, key: &Q) -> bool
        where
            T: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            match self.inner.remove(key) {
                None => false,
                Some(_) => true,
            }
        }

        pub fn insert(&mut self, key: T) -> bool
        where
            T: Ord,
        {
            match self.inner.insert(key, ()) {
                None => false,
                Some(_) => true,
            }
        }

        pub fn get<Q>(&mut self, key: &Q) -> Option<&T>
        where
            T: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.inner.get_key_value(key).map(|v| v.0)
        }

        pub fn contains<Q>(&self, key: &Q) -> bool
        where
            T: std::borrow::Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.inner.contains_key(key)
        }
    }

    impl<T> Default for BstSet<T> {
        fn default() -> Self {
            BstSet {
                inner: Default::default(),
            }
        }
    }

    impl<T: std::cmp::Ord> From<BTreeSet<T>> for BstSet<T> {
        fn from(s: BTreeSet<T>) -> Self {
            s.into_iter().collect()
        }
    }

    impl<T: std::cmp::Ord> From<BstSet<T>> for BTreeSet<T> {
        fn from(s: BstSet<T>) -> Self {
            s.into_iter().collect()
        }
    }

    impl<T> IntoIterator for BstSet<T> {
        type IntoIter = IntoIter<T>;
        type Item = T;

        fn into_iter(self) -> Self::IntoIter {
            IntoIter {
                inner: self.inner.into_iter(),
            }
        }
    }

    impl<'a, T> IntoIterator for &'a BstSet<T> {
        type IntoIter = Iter<'a, T>;
        type Item = &'a T;

        fn into_iter(self) -> Self::IntoIter {
            self.iter()
        }
    }

    impl<T> std::iter::FromIterator<T> for BstSet<T>
    where
        T: Ord,
    {
        fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
            let mut ret = Self::default();
            for i in iter {
                ret.insert(i);
            }
            ret
        }
    }

    impl<T: Ord> Extend<T> for BstSet<T> {
        fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
            for i in iter {
                self.insert(i);
            }
        }
    }

    impl<T: PartialEq> PartialEq for BstSet<T> {
        fn eq(&self, other: &Self) -> bool {
            if self.len() != other.len() {
                return false;
            }

            let mut a = self.iter();
            let mut b = other.iter();
            while let (Some(va), Some(vb)) = (a.next(), b.next()) {
                if va != vb {
                    return false;
                }
            }
            true
        }
    }

    impl<T: Eq> Eq for BstSet<T> {}

    pub struct Iter<'a, T>
    where
        T: 'a,
    {
        pub inner: super::map::Iter<'a, T, ()>,
    }

    pub struct IterMut<'a, T>
    where
        T: 'a,
    {
        pub inner: super::map::IterMut<'a, T, ()>,
    }

    pub struct IntoIter<T> {
        pub inner: super::map::IntoIter<T, ()>,
    }

    impl<'a, T> Iterator for Iter<'a, T> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|v| v.0)
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
    }

    impl<'a, T> ExactSizeIterator for Iter<'a, T> {
        fn len(&self) -> usize {
            self.inner.len()
        }
    }

    impl<'a, T> std::iter::FusedIterator for Iter<'a, T> {}

    impl<'a, T> Iterator for IterMut<'a, T> {
        type Item = &'a mut T;

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|v| v.0)
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
    }

    impl<'a, T> ExactSizeIterator for IterMut<'a, T> {
        fn len(&self) -> usize {
            self.inner.len()
        }
    }

    impl<'a, T> std::iter::FusedIterator for IterMut<'a, T> {}

    impl<T> Iterator for IntoIter<T> {
        type Item = T;

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next().map(|v| v.0)
        }

        fn size_hint(&self) -> (usize, Option<usize>) {
            self.inner.size_hint()
        }
    }

    impl<T> ExactSizeIterator for IntoIter<T> {
        fn len(&self) -> usize {
            self.inner.len()
        }
    }

    impl<T> std::iter::FusedIterator for IntoIter<T> {}

    #[cfg(test)]
    mod test {
        use super::BstSet;

        #[test]
        fn default() {
            let m: BstSet<u8> = BstSet::default();
            assert_eq!(m.len(), 0);
            assert!(m.is_empty());
        }

        #[test]
        fn insert() {
            let mut m: BstSet<u8> = BstSet::new();
            assert_eq!(m.insert(5), false);
            assert_eq!(m.insert(2), false);
            assert_eq!(m.insert(3), false);
            assert_eq!(m.insert(8), false);
            assert_eq!(m.insert(1), false);
            assert_eq!(m.len(), 5);
            assert!(!m.is_empty());
            assert_eq!(m.insert(2), true);
            assert_eq!(m.len(), 5);
        }

        #[test]
        fn get() {
            let mut m: BstSet<u8> = BstSet::new();
            assert_eq!(m.insert(5), false);
            assert_eq!(m.insert(2), false);
            assert_eq!(m.get(&1), None);
            assert_eq!(m.get(&2), Some(&2));
            assert_eq!(m.get(&5), Some(&5));
        }

        #[test]
        fn contains() {
            let mut m: BstSet<u8> = BstSet::new();
            assert_eq!(m.insert(5), false);
            assert_eq!(m.contains(&2), false);
            assert_eq!(m.contains(&5), true);
        }

        #[test]
        fn iter() {
            let mut m: BstSet<u8> = BstSet::new();
            assert_eq!(m.insert(5), false);
            assert_eq!(m.insert(2), false);
            assert_eq!(m.insert(3), false);
            assert_eq!(m.insert(8), false);
            assert_eq!(m.insert(1), false);

            let entries: Vec<_> = m.iter().collect();
            assert_eq!(entries.len(), m.len());
            let expected: Vec<&u8> = vec![&1, &2, &3, &5, &8];
            assert_eq!(entries, expected);
        }

        #[test]
        fn into_iter() {
            let mut m: BstSet<u8> = BstSet::new();
            assert_eq!(m.insert(5), false);
            assert_eq!(m.insert(2), false);
            assert_eq!(m.insert(3), false);
            assert_eq!(m.insert(8), false);
            assert_eq!(m.insert(1), false);

            let entries: Vec<_> = m.into_iter().collect();
            assert_eq!(entries.len(), 5);
            let expected: Vec<u8> = vec![1, 2, 3, 5, 8];
            assert_eq!(entries, expected);
        }

        #[test]
        fn from_iter() {
            use std::iter::FromIterator;
            let entries = vec![1, 5, 4, 23, 7, 2];
            let m = <BstSet<u8>>::from_iter(entries);
            assert_eq!(m.into_iter().collect::<Vec<_>>(), vec![1, 2, 4, 5, 7, 23]);
        }

        #[test]
        fn remove() {
            let mut m: BstSet<u8> = BstSet::new();
            assert_eq!(m.insert(5), false);
            assert_eq!(m.insert(2), false);
            assert_eq!(m.insert(3), false);
            assert_eq!(m.insert(8), false);
            assert_eq!(m.insert(1), false);

            assert_eq!(m.remove(&6), false);
            assert_eq!(m.remove(&3), true);
            assert_eq!(m.len(), 4);
            assert_eq!(m.remove(&3), false);
            assert_eq!(m.remove(&1), true);
            assert_eq!(m.len(), 3);
            m.clear();
            assert!(m.is_empty());
        }
    }
}
