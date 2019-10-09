//! Plugin registry and loading.

use libloading as dl;
use log::{info,debug};
use std::collections::HashMap;
use std::path::Path;

use super::{PluginLoadStatus,PluginRegistryIndirect,ExtensionId,SString};

/// A loaded library and the original path from which it was loaded.
struct Plugin<'a> {
    path: &'a Path,
    lib: dl::Library,
}

/// Separate struct tracking a dl::Symbol, since dl::Symbol takes a reference to dl::Library.
///
/// Could use the `rental` crate to put it in the `Plugin` struct.
struct PluginRegister<'lib>(
    dl::Symbol<'lib, unsafe extern "C" fn(&mut PluginRegistryIndirect<'_>) -> PluginLoadStatus>,
);

impl<'a> Plugin<'a> {
    /// Load the given path as a plugin.
    pub fn load(path: &'a Path) -> dl::Result<Plugin<'a>> {
        Ok(Plugin {
            path: path,
            lib: dl::Library::new(path)?,
        })
    }

    /// Get the registration function.
    pub fn register<'lib>(&'lib self) -> dl::Result<PluginRegister<'lib>> {
        unsafe { Ok(PluginRegister(self.lib.get(b"_tack_register_plugin")?)) }
    }

    /// Eject the stored library.
    pub fn eject(self) -> dl::Library {
        self.lib
    }
}


impl<'lib> PluginRegister<'lib> {
    /// Try to register the plugin.
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

/// A registry of plugin extension points.
///
/// The registry is passed to plugins sequentially when loading. Plugins can use the registry to
/// look up other extension points or add their own.
#[derive(Debug, Default)]
pub struct PluginRegistry {
    registered: HashMap<ExtensionId, *mut ()>,
    change: bool,
    plugins: Vec<dl::Library>,
    strings: Vec<String>,
}

impl PluginRegistry {
    /// Add the given extension with id to the registry.
    pub extern "C" fn add(&mut self, id: &ExtensionId, ext: *mut ()) {
        self.registered.insert(id.clone(), ext);
        debug!("added extension {:?}", id);
        self.change = true;
    }

    /// Get the extension with id from the registry.
    pub extern "C" fn get(&self, id: &ExtensionId) -> *mut () {
        *self.registered.get(id).unwrap_or(&std::ptr::null_mut())
    }

    /// Mark that a change to the registry contents occurred.
    pub extern "C" fn mark_change(&mut self) {
        debug!("marking change");
        self.change = true;
    }

    /// Add a string to the string table.
    pub extern "C" fn make_string(&mut self, bytes: *const u8, size: usize) -> SString {
        let slice = unsafe { std::slice::from_raw_parts(bytes, size) };
        match String::from_utf8(Vec::from(slice)) {
            Ok(s) => {
                debug!("created string '{}'...", s);
                self.strings.push(s);
                let ret = SStringFixed::_Offset as SString + self.strings.len() - 1;
                debug!("... with id {}", ret);
                ret
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
            info!("loading plugin from path {}", path.display());
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
                debug!("trying to register plugin {}", to_load[i].path.display());
                match entries[i].try_register(&mut PluginRegistryIndirect::new(self)) {
                    PluginLoadStatus::Success => {
                        debug!("received success");
                        to_load.remove(i);
                        entries.remove(i);
                        ()
                    }
                    PluginLoadStatus::WaitOptional => {
                        debug!("received optional wait");
                        i += 1
                    }
                    PluginLoadStatus::WaitRequired(s) => {
                        debug!("received required wait");
                        match self.resolve_sstring(s) {
                            Ok(st) => required.push(format!(
                                "{} failed to load due to requirement: {}",
                                to_load[i].path.display(),
                                st
                            )),
                            Err(st) => {
                                return Err(format!(
                                    "{} failed to load: {}",
                                    to_load[i].path.display(),
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
                            to_load[i].path.display(),
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

