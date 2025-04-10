use std::collections::HashMap;

pub trait ContextGlobal: 'static {
    fn as_any(&self) -> &dyn std::any::Any;
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any;
}

pub(crate) struct ContextGlobalsSet {
    globals_map: HashMap<std::any::TypeId, Box<dyn ContextGlobal>>,
}

impl ContextGlobalsSet {
    // Create an empty set.
    pub fn new() -> Self {
        Self {
            globals_map: HashMap::new(),
        }
    }

    /// Get the object of type T, or None if not found.
    pub fn get<T: ContextGlobal>(&self) -> Option<&T> {
        let entry = self.globals_map.get(&std::any::TypeId::of::<T>())?;
        Some(entry.as_any().downcast_ref::<T>().unwrap())
    }

    /// Get the mutable object of type T, or None if not found.
    pub fn get_mut<T: ContextGlobal>(&mut self) -> Option<&mut T> {
        let entry = self.globals_map.get_mut(&std::any::TypeId::of::<T>())?;
        Some(entry.as_any_mut().downcast_mut::<T>().unwrap())
    }

    /// Returns true if T is in the set.
    pub fn contains<T: ContextGlobal>(&self) -> bool {
        self.globals_map.contains_key(&std::any::TypeId::of::<T>())
    }

    /// Try to insert `obj` in the set.
    /// Won't do anything is there is already a global in the set.
    pub fn insert<T: ContextGlobal>(&mut self, obj: T) -> &mut T {
        let entry = self
            .globals_map
            .entry(std::any::TypeId::of::<T>())
            .or_insert(Box::new(obj));
        entry.as_any_mut().downcast_mut::<T>().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MyTypeA {
        x: i64,
    }

    impl ContextGlobal for MyTypeA {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
            self
        }
    }

    struct MyTypeB {
        x: i64,
    }

    impl ContextGlobal for MyTypeB {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
            self
        }
    }

    #[test]
    fn test_globals_set() {
        let mut globals = ContextGlobalsSet::new();
        assert!(globals.get::<MyTypeA>().is_none());
        assert!(globals.get::<MyTypeB>().is_none());
        globals.insert(MyTypeA { x: 42 });
        assert!(globals.get::<MyTypeA>().is_some());
        assert!(globals.get::<MyTypeB>().is_none());
        globals.insert(MyTypeB { x: 7 });
        assert_eq!(globals.get::<MyTypeA>().unwrap().x, 42);
        assert_eq!(globals.get::<MyTypeB>().unwrap().x, 7);

        // Inserting a new A won't work.
        globals.insert(MyTypeA { x: 1000 });
        assert_eq!(globals.get::<MyTypeA>().unwrap().x, 42);
        assert_eq!(globals.get::<MyTypeB>().unwrap().x, 7);
    }
}
