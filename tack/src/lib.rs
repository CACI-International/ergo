//! Tack plugin system.
//!
//! One should add plugin entrypoints using the [`register_plugin`] macro.
//!
//! Plugins should be built as `cdylib` targets.
//!
//! # Safety
//! It is up to the plugin implementation to ensure ABI compatibility, and in particular forward
//! compatibility should be kept in mind. Thus, any data types used as, or in, extension points
//! should be `#[repr(C)]` or, in the case of functions, `extern` to use the C calling convention.
//!
//! # Compatibility
//! While this interface has an ABI that is C FFI safe, it is recommended that plugins be written
//! in rust and use this crate to ease interfacing.
//!
//! [`register_plugin`]: ../macro.bob_register_plugin.html

#[cfg(feature = "registry")]
mod registry;

#[cfg(feature = "registry")]
pub use self::registry::PluginRegistry;
#[cfg(not(feature = "registry"))]
struct PluginRegistry;

/// Type of extension ids. These should generally be Uuids to avoid collisions.
pub type ExtensionId = [u8; 16];
/// String identifiers, used to return strings as part of plugin loading status.
pub type SString = usize;

/// Rust extension types.
///
/// Extension types statically provide a related extension id.
pub trait Extension {
    /// The extension id for the extension type.
    fn extension_id() -> ExtensionId;
}


/// The plugin loading status of a particular plugin.
#[repr(C)]
#[derive(Debug)]
pub enum PluginLoadStatus {
    /// The plugin was loaded successfully.
    Success,
    /// The plugin is waiting for an optional dependency.
    WaitOptional,
    /// The plugin is waiting for a required dependency.
    WaitRequired(SString),
    /// The plugin failed to load.
    Fail(SString),
}

/// A macro to register a plugin entrypoint.
///
/// The macro content should be an identifier to use for the registry followed by a fat arrow and
/// an expression (likely a block) that evaluates to a PluginLoadStatus. The expression has the
/// given identifier in scope as a mutable [`PluginRegistryInterface`].
///
/// A simple example registering an extension (that's fairly useless):
/// ```
/// use tack::{register_plugin,Extension,ExtensionId};
///
/// ##[repr(C)]
/// ##[derive(Default)]
/// struct MyExtension {}
///
/// impl Extension for MyExtension {
///     fn extension_id() -> ExtensionId { [1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4] }
/// }
///
/// register_plugin! { registry => {
///     let ext: &'static mut MyExtension = Box::leak(Box::new(MyExtension::default()));
///     registry.add(ext);
///     PluginLoadStatus::Success
/// }}
/// ```
///
/// [`PluginRegistryInterface`]: ./plugin/struct.PluginRegistryInterface.html
#[macro_export]
macro_rules! register_plugin {
    ( $p:ident => $x:expr ) => {
        use $crate::{PluginLoadStatus, PluginRegistryIndirect, PluginRegistryInterface};

        #[no_mangle]
        pub extern "C" fn _tack_register_plugin(
            _indirect: &mut PluginRegistryIndirect<'_>,
        ) -> PluginLoadStatus {
            match PluginRegistryInterface::new(_indirect) {
                Ok(mut $p) => $x,
                Err(e) => PluginLoadStatus::Fail(0),
            }
        }
    };
}


const PLUGIN_REGISTRY_INTERFACE_SIZE: usize = 4;

#[repr(C)]
pub struct PluginRegistryIndirect<'a> {
    data: &'a mut PluginRegistry,
    size: usize,
    add: extern "C" fn(&mut PluginRegistry, &ExtensionId, *mut ()),
    get: extern "C" fn(&PluginRegistry, &ExtensionId) -> *mut (),
    mark_change: extern "C" fn(&mut PluginRegistry),
    make_string: extern "C" fn(&mut PluginRegistry, *const u8, usize) -> SString,
}

impl<'a> PluginRegistryIndirect<'a> {
    #[cfg(feature = "registry")]
    pub fn new(registry: &'a mut PluginRegistry) -> PluginRegistryIndirect {
        PluginRegistryIndirect {
            data: registry,
            size: PLUGIN_REGISTRY_INTERFACE_SIZE,
            add: PluginRegistry::add,
            get: PluginRegistry::get,
            mark_change: PluginRegistry::mark_change,
            make_string: PluginRegistry::make_string,
        }
    }
}

/// An interface to the plugin registry.
///
/// Accesses the registry functions in an ABI-stable and forward-compatible way.
pub struct PluginRegistryInterface<'a, 'b>(&'a mut PluginRegistryIndirect<'b>);

impl<'a, 'b> PluginRegistryInterface<'a, 'b> {
    pub fn new(indirect: &'a mut PluginRegistryIndirect<'b>) -> Result<Self, String> {
        if indirect.size < PLUGIN_REGISTRY_INTERFACE_SIZE {
            Err("plugin interface incompatible".to_owned())
        } else {
            Ok(PluginRegistryInterface(indirect))
        }
    }

    /// Add a plugin extension point to the registry.
    ///
    /// References must have a static lifetime. As these should generally exist for the lifetime
    /// of the application, it is easiest to create such a reference with a leaked Box:
    /// ```
    /// # struct MyExtension {}
    /// let ext: &'static mut MyExtension = Box::leak(Box::new(MyExtension{}));
    /// ```
    pub fn add<T: Extension>(&mut self, ext: &'static mut T) {
        (self.0.add)(
            self.0.data,
            &T::extension_id(),
            (ext as *mut T) as *mut (),
        );
        self.mark_change();
    }

    /// Get a plugin extension point from the registry.
    ///
    /// The returned mutable reference is accessed with unsafe from a stored pointer
    /// for type erasure, but as the PluginRegistry is passed around as a mutable reference,
    /// borrow safety is ensured.
    pub fn get<T: Extension>(&self) -> Option<&mut T> {
        let ptr = (self.0.get)(self.0.data, &T::extension_id());
        unsafe { (ptr as *mut T).as_mut() }
    }

    /// Mark that a change to the registry has occurred.
    pub fn mark_change(&mut self) {
        (self.0.mark_change)(self.0.data);
    }

    /// Create a string to be used in return values.
    pub fn make_string(&mut self, s: &str) -> SString {
        (self.0.make_string)(self.0.data, s.as_ptr(), s.len())
    }
}

#[cfg(all(test,feature = "registry"))]
mod tests {
    #[test]
    fn test_plugin_loading() {
        use std::path::Path;

        let mut registry = super::PluginRegistry::default();

        // Find the basic_plugin in the examples directory
        let path = Path::new(if cfg!(debug_assertions) {
            "target/debug/examples"
        } else {
            "target/release/examples"
        });
        let plugin_path = path
            .read_dir()
            .expect("read_dir failed")
            .find_map(|entry| {
                let e = entry.expect("entry failed");
                if e.file_type().expect("file_type failed").is_file()
                    && e.path()
                        .file_name()
                        .and_then(|s| s.to_str())
                        .map(|s| s == "libbasic_plugin.so")
                        .unwrap_or_default()
                {
                    Some(e.path())
                } else {
                    None
                }
            })
            .expect("could not find basic_plugin path");
        match registry.load(vec![plugin_path.as_path()]) {
            Ok(()) => (),
            Err(s) => panic!(s),
        }
    }
}

