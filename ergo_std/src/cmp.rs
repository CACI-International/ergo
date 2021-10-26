//! Comparison types.

use ergo_runtime::abi_stable::{std_types::RCmpOrdering, StableAbi};
use ergo_runtime::{traits, type_system::ErgoType, Value};

pub fn module() -> Value {
    crate::make_string_map! {
        "less" = less_val(),
        "equal" = equal_val(),
        "greater" = greater_val()
    }
}

fn less_val() -> Value {
    Order::from(std::cmp::Ordering::Less).into()
}

fn equal_val() -> Value {
    Order::from(std::cmp::Ordering::Equal).into()
}

fn greater_val() -> Value {
    Order::from(std::cmp::Ordering::Greater).into()
}

#[derive(ErgoType, StableAbi, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
#[repr(C)]
pub struct Order(pub RCmpOrdering);

impl From<std::cmp::Ordering> for Order {
    fn from(o: std::cmp::Ordering) -> Self {
        Order(o.into())
    }
}

impl From<Order> for std::cmp::Ordering {
    fn from(o: Order) -> Self {
        o.0.into_ordering()
    }
}

impl From<Order> for ergo_runtime::TypedValue<Order> {
    fn from(e: Order) -> Self {
        ergo_runtime::TypedValue::constant(e)
    }
}

impl From<&'_ Order> for ergo_runtime::Dependencies {
    fn from(o: &'_ Order) -> Self {
        ergo_runtime::depends![Order::ergo_type(), o]
    }
}

impl std::fmt::Display for Order {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            RCmpOrdering::Less => write!(f, "Order:less"),
            RCmpOrdering::Equal => write!(f, "Order:equal"),
            RCmpOrdering::Greater => write!(f, "Order:greater"),
        }
    }
}

ergo_runtime::HashAsDependency!(Order);

ergo_runtime::type_system::ergo_traits_fn! {
    ergo_runtime::ergo_type_name!(traits, Order);
    ergo_runtime::ergo_display_basic!(traits, Order);
    traits::ValueByContent::add_impl::<Order>(traits);
}
