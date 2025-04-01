use std::collections::HashMap;

struct TreeEntry {
    // True if the value is in the map.
    is_end: bool,
    // Map from children => index.
    children: HashMap<char, usize>,
}

// Data Structure used to hold a set of words.
// We can easily find a subset of a string with it.
pub struct DenseTrie {
    // The tree is a represented as a vector en entries.
    tree: Vec<TreeEntry>,

    // Path of characters taken so far.
    path: Vec<(char, usize)>,
}

impl DenseTrie {
    // Create an empty trie.
    pub fn new() -> Self {
        Self {
            tree: vec![TreeEntry {
                is_end: false,
                children: HashMap::new(),
            }],
            path: vec![],
        }
    }

    // Insert a new word into the trie.
    pub fn insert(&mut self, word: &str) {
        let mut idx = 0;
        let mut tree_size = self.tree.len();

        for c in word.chars() {
            let entry = &mut self.tree[idx];
            if let Some(next_idx) = entry.children.get(&c) {
                idx = *next_idx;
            } else {
                entry.children.insert(c, tree_size);
                idx = tree_size;
                self.tree.push(TreeEntry {
                    is_end: false,
                    children: HashMap::new(),
                });
                tree_size += 1;
            }
        }

        self.tree[idx].is_end = true;
    }

    // Returns true if `word` is inside the trie.
    pub fn contains(&self, word: &str) -> bool {
        let mut idx = 0;

        for c in word.chars() {
            idx = match self.tree[idx].children.get(&c) {
                Some(idx) => *idx,
                None => return false,
            };
        }

        true
    }

    // Returns true if any of the words in the trie starts with `c`.
    pub fn any_starts_with(&self, c: char) -> bool {
        self.tree[0].children.contains_key(&c)
    }

    // Reset the current path
    pub fn reset_path(&mut self) {
        self.path.clear();
    }

    // Try to advance of one char the current path.
    // Returns None if we can't advance (no existing prefix).
    // Or returns `is_end` for the new prefix.
    pub fn try_advance(&mut self, c: char) -> Option<bool> {
        let idx = match self.path.last() {
            Some((_c, idx)) => *idx,
            None => 0,
        };
        let entry = &self.tree[idx];
        let next_idx = match entry.children.get(&c) {
            Some(idx) => *idx,
            None => return None,
        };

        let is_end = self.tree[next_idx].is_end;
        self.path.push((c, next_idx));
        Some(is_end)
    }
}
