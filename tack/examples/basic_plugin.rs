use tack::{register_plugin,Extension,ExtensionId};

#[derive(Default)]
struct MyExtension {
    value: i32,
}

impl Extension for MyExtension {
    fn extension_id() -> ExtensionId {
        [1,2,3,4,1,2,3,4,1,2,3,4,5,6,7,8]
    }
}

register_plugin! { registry => {
    if let Some(ref mut r) = registry.get::<MyExtension>() {
        r.value = 42;
        PluginLoadStatus::Success
    } else {
        let ext: &'static mut MyExtension = Box::leak(Box::new(MyExtension::default()));
        registry.add(ext);
        PluginLoadStatus::WaitRequired(registry.make_string("need MyExtension plugin"))
    }
}}
