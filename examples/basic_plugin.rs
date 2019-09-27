use bob::bob_register_plugin;
use bob::uuid as id;
use uuid::Uuid;

#[derive(Default)]
struct MyExtension {
    value: i32,
}

impl id::UniqueType for MyExtension {
    fn type_id() -> Uuid {
        Uuid::new_v5(&*id::PLUGIN_NS, b"myextension")
    }
}

bob_register_plugin! { registry => {
    if let Some(ref mut r) = registry.get::<MyExtension>() {
        r.value = 42;
        PluginLoadStatus::Success
    } else {
        let ext: &'static mut MyExtension = Box::leak(Box::new(MyExtension::default()));
        registry.add(ext);
        PluginLoadStatus::WaitRequired(registry.make_string("need MyExtension plugin"))
    }
}}
