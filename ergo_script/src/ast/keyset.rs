//! A set over specific keys generated from a context.

type Storage = usize;
type KeyType = usize;

/// The shared context over sets sharing the same keys.
#[derive(Debug)]
pub struct Context {
    next_key: KeyType,
}

/// A key that can be included in sets.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Key(pub KeyType);

impl Default for Context {
    fn default() -> Self {
        Context { next_key: 0 }
    }
}

impl Context {
    pub fn key(&mut self) -> Key {
        let ret = Key(self.next_key);
        self.next_key += 1;
        ret
    }
}

/// A set of keys from a single context.
///
/// # Safety
/// It is up to the programmer to ensure that the keys used are from a single Context.
#[derive(Default, Clone, Eq)]
pub struct KeySet {
    contains: Vec<Storage>,
}

impl std::fmt::Debug for KeySet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "KeySet {{")?;
        let mut first = true;
        for k in self.iter() {
            if !first {
                write!(f, ",")?;
            }
            first = false;
            k.0.fmt(f)?;
        }
        write!(f, "}}")
    }
}

struct ZipSetIter<'a> {
    a: std::slice::Iter<'a, Storage>,
    b: std::slice::Iter<'a, Storage>,
}

impl<'a> ZipSetIter<'a> {
    pub fn new(a: &'a KeySet, b: &'a KeySet) -> Self {
        ZipSetIter {
            a: a.contains.iter(),
            b: b.contains.iter(),
        }
    }
}

impl<'a> Iterator for ZipSetIter<'a> {
    type Item = (Storage, Storage);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.a.next(), self.b.next()) {
            (Some(a), Some(b)) => Some((*a, *b)),
            (Some(a), None) => Some((*a, 0)),
            (None, Some(b)) => Some((0, *b)),
            (None, None) => None,
        }
    }
}

impl PartialEq for KeySet {
    fn eq(&self, other: &Self) -> bool {
        ZipSetIter::new(self, other).all(|(a, b)| a == b)
    }
}

impl PartialOrd for KeySet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for KeySet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.iter().cmp(other.iter())
    }
}

impl std::hash::Hash for KeySet {
    fn hash<H: std::hash::Hasher>(&self, h: &mut H) {
        for k in self.iter() {
            k.hash(h);
        }
    }
}

const BITS: KeyType = Storage::BITS as KeyType;

#[allow(dead_code)]
impl KeySet {
    pub fn clear(&mut self) {
        self.contains.clear()
    }

    pub fn insert(&mut self, k: Key) {
        let offset = k.0 / BITS;
        let bit = k.0 % BITS;
        if offset >= self.contains.len() {
            self.contains.resize(offset + 1, 0);
        }
        self.contains[offset] |= 1usize << bit;
    }

    pub fn remove(&mut self, k: Key) {
        let offset = k.0 / BITS;
        if offset >= self.contains.len() {
            return;
        }
        let bit = k.0 % BITS;
        self.contains[offset] &= !(1usize << bit);
    }

    pub fn contains(&self, k: Key) -> bool {
        let offset = k.0 / BITS;
        if offset >= self.contains.len() {
            return false;
        }
        let bit = k.0 % BITS;
        (self.contains[offset] & (1usize << bit)) != 0
    }

    pub fn iter(&self) -> Iter {
        Iter {
            contains: self.contains.iter().peekable(),
            key: 0,
        }
    }

    pub fn len(&self) -> usize {
        let mut l = 0usize;
        for s in self.contains.iter() {
            l += s.count_ones() as usize;
        }
        l
    }

    pub fn pop(&mut self) -> Option<Key> {
        self.iter().next().map(|key| {
            self.remove(key);
            key
        })
    }

    pub fn contains_set(&self, other: &Self) -> bool {
        ZipSetIter::new(self, other).all(|(a, b)| a & b == b)
    }

    pub fn union_with(&mut self, other: &Self) {
        if self.contains.len() < other.contains.len() {
            self.contains.resize(other.contains.len(), 0);
        }
        for (offset, val) in other.contains.iter().enumerate() {
            self.contains[offset] |= val;
        }
    }

    pub fn intersect_with(&mut self, other: &Self) {
        for (offset, val) in other.contains.iter().take(self.contains.len()).enumerate() {
            self.contains[offset] &= val;
        }
    }

    pub fn difference_with(&mut self, other: &Self) {
        for (offset, val) in other.contains.iter().take(self.contains.len()).enumerate() {
            self.contains[offset] &= !val;
        }
    }
}

impl std::ops::BitOrAssign<&'_ Self> for KeySet {
    fn bitor_assign(&mut self, other: &'_ Self) {
        self.union_with(other);
    }
}

impl std::ops::BitOrAssign<&'_ KeySet> for &'_ mut KeySet {
    fn bitor_assign(&mut self, other: &'_ KeySet) {
        self.union_with(other);
    }
}

impl std::iter::FromIterator<Key> for KeySet {
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Key>,
    {
        let mut set = Self::default();
        for k in iter.into_iter() {
            set.insert(k);
        }
        set
    }
}

pub struct Iter<'a> {
    contains: std::iter::Peekable<std::slice::Iter<'a, Storage>>,
    key: KeyType,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Key;

    fn next(&mut self) -> Option<Self::Item> {
        'outer: loop {
            match self.contains.peek() {
                None => break None,
                Some(0) => {
                    // Fast skip
                    self.key += BITS;
                    self.contains.next();
                }
                Some(&v) => loop {
                    let bit = self.key % BITS;
                    let set = (*v & (1usize << bit)) != 0;
                    let ret = self.key;
                    self.key += 1;
                    let next = self.key % BITS == 0;
                    if next {
                        self.contains.next();
                    }

                    if set {
                        break 'outer Some(Key(ret));
                    } else if next {
                        break;
                    }
                },
            }
        }
    }
}
