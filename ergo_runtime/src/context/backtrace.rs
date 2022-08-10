//! Evaluation backtrace storage.

use crate::abi_stable::{
    std_types::{RArc, ROption, RString},
    StableAbi,
};
use crate::source::Source;

#[derive(Clone, Debug, Default, StableAbi)]
#[repr(C)]
pub struct Backtrace {
    stack: ROption<RArc<Frame>>,
}

impl Backtrace {
    /// Push a frame onto the current backtrace.
    pub fn push<S: Into<RString>>(&mut self, frame: Source<S>) {
        self.stack = ROption::RSome(RArc::new(Frame {
            last: self.stack.take(),
            content: frame.map(|s| s.into()),
        }));
    }

    /// Iterate over the current backtrace (ordering frames from most to least recent).
    pub fn iter(&self) -> Iter {
        Iter(&self.stack)
    }
}

pub struct Iter<'a>(&'a ROption<RArc<Frame>>);

impl<'a> Iterator for Iter<'a> {
    type Item = &'a Source<RString>;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.0 {
            ROption::RNone => None,
            ROption::RSome(frame) => {
                self.0 = &frame.last;
                Some(&frame.content)
            }
        }
    }
}

#[derive(Debug, StableAbi)]
#[repr(C)]
struct Frame {
    last: ROption<RArc<Frame>>,
    content: Source<RString>,
}
