use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
#[derive(Default, Debug, Clone)]
pub struct StackedMap<K: Hash + Eq, V: Clone> {
    table: HashMap<K, Vec<V>>,
    scopes: Vec<Option<K>>,
}

impl<K: Hash + Eq + Copy + Clone, V: Clone + PartialEq> PartialEq for StackedMap<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.table == other.table && self.scopes == other.scopes
    }
}

impl<K: Hash + Eq + Copy + Clone, V: Clone + PartialEq> Eq for StackedMap<K, V> {}

impl<K: Hash + Eq + Copy + Clone, V: Clone> StackedMap<K, V> {
    pub fn new() -> Self {
        StackedMap {
            table: HashMap::new(),
            scopes: vec![],
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(None);
    }

    pub fn end_scope(&mut self) {
        while let Some(Some(value)) = self.scopes.pop() {
            let mapping = self.table.get_mut(&value).expect("Symbol not in Symbols");
            mapping.pop();
        }
    }

    /// Enters a piece of data into the current scope
    pub fn insert(&mut self, key: K, value: V) {
        let mapping = self.table.entry(key).or_insert_with(Vec::new);
        mapping.push(value);

        self.scopes.push(Some(key));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        let _depth = self.scopes.len();

        self.table.get(key).and_then(|vec| vec.last())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_it_works() {
        let mut map = StackedMap::new();

        map.insert(1, "a");
        map.begin_scope();
        map.insert(1, "b");
        assert_eq!(map.get(&1), Some(&"b"));
        map.end_scope();
        assert_eq!(map.get(&1), Some(&"a"));
    }

    #[test]
    fn test_get() {
        let mut map = StackedMap::new();

        map.insert(1, "b");
        assert_eq!(map.get(&1), Some(&"b"));
        map.end_scope();
        assert_eq!(map.get(&2), None);
    }
}
