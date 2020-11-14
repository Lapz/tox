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

    pub fn end_scope_iter(&mut self) -> ExitScopeIter<K, V> {
        ExitScopeIter {
            map: self,
            done: false,
        }
    }

    /// Enters a piece of data into the current scope
    pub fn insert(&mut self, key: K, value: V) {
        let mapping = self.table.entry(key).or_insert_with(Vec::new);
        mapping.push(value);

        self.scopes.push(Some(key));
    }

    pub fn update(&mut self, key: K, value: V) {
        let mapping = self.table.entry(key).or_insert_with(Vec::new);
        if mapping.is_empty() {
            mapping.push(value);
        } else {
            mapping.pop();
            mapping.push(value);
        }
    }

    pub fn is_in_scope(&self, key: &K) -> bool {
        for v in self.scopes.iter().rev() {
            match v {
                Some(ref n) if n == key => return true,
                Some(_) => (),
                None => break,
            }
        }

        false
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.table.get(key).and_then(|vec| vec.last())
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.table.get_mut(key).and_then(|vec| vec.last_mut())
    }
}

#[cfg(test)]
mod tests {
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
    fn test_scope() {
        let mut map = StackedMap::new();

        map.insert(1, "b");
        map.begin_scope();
        assert_eq!(map.get(&1), Some(&"b"));
        map.end_scope();
        assert_eq!(map.get(&1), Some(&"b"));
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

pub struct ExitScopeIter<'a, K, V>
where
    K: 'a + Eq + Hash,
    V: 'a + Clone,
{
    map: &'a mut StackedMap<K, V>,
    done: bool,
}

impl<'a, K, V: Clone> Iterator for ExitScopeIter<'a, K, V>
where
    K: Eq + Hash + Clone,
{
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            None
        } else {
            match self.map.scopes.pop() {
                Some(Some(key)) => {
                    let result = self.map.table.get_mut(&key).and_then(|x| x.pop());
                    self.done = result.is_none();
                    result.map(|value| (key, value))
                }
                _ => {
                    self.done = true;
                    None
                }
            }
        }
    }
}
