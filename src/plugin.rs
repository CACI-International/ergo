//! Plugin system.
//!
//! One should add plugin entrypoints using the [`bob_register_plugin`] macro.
//!
//! Plugins should be built as `cdylib` targets.
//!
//! # Safety
//! It is up to the plugin implementation to ensure ABI compatibility, and in particular forward
//! compatibility should be kept in mind. Thus, any data types used as, or in, extension points
//! should be `#[repr(C)]` or, in the case of functions, `extern` to use the C calling convention.
//!
//! [`bob_register_plugin`]: ../macro.bob_register_plugin.html

use crate::uuid::UniqueType;
use libloading as dl;
use std::collections::HashMap;
use std::path::Path;

pub type ExtensionId = [u8; 16];
pub type SString = usize;

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

struct Plugin<'a> {
    path: &'a Path,
    lib: dl::Library,
}

/// A macro to register a bob plugin entrypoint.
///
/// The macro content should be an identifier to use for the registry followed by a fat arrow and
/// an expression (likely a block) that evaluates to a PluginLoadStatus. The expression has the
/// given identifier in scope as a mutable [`PluginRegistryInterface`].
///
/// A simple example registering an extension (that's fairly useless):
/// ```
/// use bob::bob_register_plugin;
/// use bob::uuid::{UniqueType,PLUGIN_NS};
/// use uuid::Uuid;
///
/// ##[repr(C)]
/// ##[derive(Default)]
/// struct MyExtension {}
///
/// impl UniqueType for MyExtension {
///   fn type_id() -> Uuid { Uuid::new_v5(&*PLUGIN_NS,b"myextension") }
/// }
///
/// bob_register_plugin! { registry => {
///     let ext: &'static mut MyExtension = Box::leak(Box::new(MyExtension::default()));
///     registry.add(ext);
///     PluginLoadStatus::Success
/// }}
/// ```
///
/// [`PluginRegistryInterface`]: ./plugin/struct.PluginRegistryInterface.html
#[macro_export]
macro_rules! bob_register_plugin {
    ( $p:ident => $x:expr ) => {
        use $crate::plugin::{PluginLoadStatus, PluginRegistryIndirect, PluginRegistryInterface};

        #[no_mangle]
        pub extern "C" fn bob_register_plugin(
            _indirect: &mut PluginRegistryIndirect<'_>,
        ) -> PluginLoadStatus {
            match PluginRegistryInterface::new(_indirect) {
                Ok(mut $p) => $x,
                Err(e) => PluginLoadStatus::Fail(0),
            }
        }
    };
}

/// Separate struct tracking a dl::Symbol, since dl::Symbol takes a reference to dl::Library.
///
/// Could use the `rental` crate to put it in the `Plugin` struct.
struct PluginRegister<'lib>(
    dl::Symbol<'lib, unsafe extern "C" fn(&mut PluginRegistryIndirect<'_>) -> PluginLoadStatus>,
);

impl<'a> Plugin<'a> {
    pub fn load(path: &'a Path) -> dl::Result<Plugin<'a>> {
        Ok(Plugin {
            path: path,
            lib: dl::Library::new(path)?,
        })
    }

    pub fn register<'lib>(&'lib self) -> dl::Result<PluginRegister<'lib>> {
        unsafe { Ok(PluginRegister(self.lib.get(b"bob_register_plugin")?)) }
    }

    pub fn path(&self) -> &'a Path {
        self.path
    }

    pub fn eject(self) -> dl::Library {
        self.lib
    }
}

impl<'lib> PluginRegister<'lib> {
    pub fn try_register(&self, registry: &mut PluginRegistryIndirect) -> PluginLoadStatus {
        unsafe { (self.0)(registry) }
    }
}

/// Fixed sstring values
enum SStringFixed {
    PluginInterfaceIncompatible = 0,
    InvalidUtf8,
    _Offset, // Must be last element in the enum
}

/**
 * A registry of plugin extension points.
 *
 * The registry is passed to plugins sequentially when loading. Plugins can use the registry to
 * look up other extension points or add their own.
 */
#[derive(Debug, Default)]
pub struct PluginRegistry {
    registered: HashMap<ExtensionId, *mut ()>,
    change: bool,
    plugins: Vec<dl::Library>,
    strings: Vec<String>,
}

impl PluginRegistry {
    /// Add the given extension with id to the registry.
    pub extern "C" fn add(&mut self, id: ExtensionId, ext: *mut ()) {
        self.registered.insert(id, ext);
        self.change = true;
    }

    /// Get the extension with id from the registry.
    pub extern "C" fn get(&self, id: &ExtensionId) -> *mut () {
        *self.registered.get(id).unwrap_or(&std::ptr::null_mut())
    }

    /// Mark that a change to the registry contents occurred.
    pub extern "C" fn mark_change(&mut self) {
        self.change = true;
    }

    /// Add a string to the string table.
    pub extern "C" fn make_string(&mut self, bytes: *const u8, size: usize) -> SString {
        let slice = unsafe { std::slice::from_raw_parts(bytes, size) };
        match String::from_utf8(Vec::from(slice)) {
            Ok(s) => {
                self.strings.push(s);
                SStringFixed::_Offset as SString + self.strings.len() - 1
            }
            Err(_) => SStringFixed::InvalidUtf8 as SString,
        }
    }

    /// Reset the change state in the registry, returning the current state.
    pub fn reset_change(&mut self) -> bool {
        let ret = self.change;
        self.change = false;
        ret
    }

    /// Resolve the given SString to a string slice.
    fn resolve_sstring(&self, s: SString) -> Result<&str, &str> {
        if s == SStringFixed::PluginInterfaceIncompatible as SString {
            Err("incompatible plugin interface")
        } else if s == SStringFixed::InvalidUtf8 as SString {
            Err("provided invalid utf8 when creating string")
        } else {
            self.strings
                .get(s - SStringFixed::_Offset as SString)
                .ok_or("provided invalid string reference").map(|s| s.as_ref())
        }
    }

    /// Load the given plugin files into the registry.
    pub fn load(&mut self, paths: Vec<&Path>) -> Result<(), String> {
        let mut plugins = Vec::with_capacity(paths.len());
        for path in &paths {
            plugins.push(Plugin::load(path).map_err(|e| format!("{}", e))?);
        }
        let mut entries = Vec::with_capacity(paths.len());
        for plugin in &plugins {
            entries.push(plugin.register().map_err(|e| format!("{}", e))?);
        }

        let mut to_load: Vec<&Plugin> = plugins.iter().collect();

        self.change = true;
        let mut required = Vec::new();
        while !to_load.is_empty() && self.change {
            self.change = false;
            self.strings.clear();
            required.clear();

            let mut i = 0;
            while i < to_load.len() {
                match entries[i].try_register(&mut PluginRegistryIndirect::new(self)) {
                    PluginLoadStatus::Success => {
                        to_load.remove(i);
                        entries.remove(i);
                        ()
                    }
                    PluginLoadStatus::WaitOptional => i += 1,
                    PluginLoadStatus::WaitRequired(s) => {
                        match self.resolve_sstring(s) {
                            Ok(st) => required.push(format!(
                                "{} failed to load due to requirement: {}",
                                to_load[i].path().display(),
                                st
                            )),
                            Err(st) => {
                                return Err(format!(
                                    "{} failed to load: {}",
                                    to_load[i].path().display(),
                                    st
                                ))
                            }
                        }
                        i += 1
                    }
                    PluginLoadStatus::Fail(s) => {
                        let string = match self.resolve_sstring(s) {
                            Ok(st) => st,
                            Err(st) => st,
                        };
                        return Err(format!(
                            "{} failed to load: {}",
                            to_load[i].path().display(),
                            string
                        ));
                    }
                }
            }
        }

        if !required.is_empty() {
            Err(required.join("\n"))
        } else {
            self.plugins.extend(plugins.into_iter().map(|p| p.eject()));
            Ok(())
        }
    }
}

const PLUGIN_REGISTRY_INTERFACE_SIZE: usize = 4;

#[repr(C)]
pub struct PluginRegistryIndirect<'a> {
    data: &'a mut PluginRegistry,
    size: usize,
    add: extern "C" fn(&mut PluginRegistry, ExtensionId, *mut ()),
    get: extern "C" fn(&PluginRegistry, &ExtensionId) -> *mut (),
    mark_change: extern "C" fn(&mut PluginRegistry),
    make_string: extern "C" fn(&mut PluginRegistry, *const u8, usize) -> SString,
}

impl<'a> PluginRegistryIndirect<'a> {
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
    pub fn add<T: UniqueType>(&mut self, ext: &'static mut T) {
        (self.0.add)(
            self.0.data,
            T::type_id().as_bytes().clone(),
            (ext as *mut T) as *mut (),
        );
        self.mark_change();
    }

    /// Get a plugin extension point from the registry.
    ///
    /// The returned mutable reference is accessed with unsafe from a stored pointer
    /// for type erasure, but as the PluginRegistry is passed around as a mutable reference,
    /// borrow safety is ensured.
    pub fn get<T: UniqueType>(&self) -> Option<&mut T> {
        let ptr = (self.0.get)(self.0.data, T::type_id().as_bytes());
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

#[cfg(test)]
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
