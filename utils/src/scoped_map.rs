use std::{collections::HashMap, hash::Hash};

// Basic implementation of an hashmap with scopes.
// Scopes open / close in a FIFO manner.
// There is no checks that different scopes contains the same key.
pub struct ScopedMap<K: Hash + Eq + Sized, V> {
    scopes: Vec<HashMap<K, V>>,
}

impl<K: Hash + Eq + Sized, V> ScopedMap<K, V> {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    // Returns the current number of scopes.
    pub fn scopes_count(&self) -> usize {
        self.scopes.len()
    }

    // Remove the whole map data.
    pub fn clear(&mut self) {
        self.scopes.clear();
    }

    // Open a new scope.
    pub fn open_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // Close the top scope.
    pub fn close_scope(&mut self) {
        assert!(self.scopes.len() > 0, "no open scopes");
        self.scopes.pop();
    }

    // Returns true if `key` is in the map.
    pub fn contains(&self, key: &K) -> bool {
        self.scopes.iter().any(|scope| scope.contains_key(key))
    }

    // Returns true if `key` is in the top scope.
    pub fn contains_in_top(&self, key: &K) -> bool {
        let scope = self.scopes.last().expect("no open scopes");
        scope.contains_key(key)
    }

    // Returns the value associated to key
    // Or returns none if not found.
    pub fn get(&self, key: &K) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.get(key) {
                return Some(res);
            }
        }

        None
    }

    // Returns the value associated to key
    // Or returns none if not found.
    // Only look in the top scope.
    pub fn get_in_top(&self, key: &K) -> Option<&V> {
        let scope = self.scopes.last().expect("no open scopes");
        scope.get(key)
    }

    // Insert the (key, value) pair at in the top scope.
    // Does nothing if it's already there.
    pub fn insert(&mut self, key: K, value: V) {
        let scope = self.scopes.last_mut().expect("no open scopes");
        scope.insert(key, value);
    }
}
