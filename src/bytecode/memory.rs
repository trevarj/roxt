use crate::object::Object;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Memory {
    heap: Vec<Object>,
    string_intern_pool: HashMap<String, usize>,
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            heap: Vec::with_capacity(512),
            string_intern_pool: HashMap::new(),
        }
    }

    /// Pushes object onto the heap and returns the index
    pub fn add_object(&mut self, obj: Object) -> usize {
        if let Object::String(str) = obj {
            self.add_string_intern(&str)
        } else {
            self.heap.push(obj);
            self.heap.len() - 1
        }
    }

    pub fn get_object_by_ptr(&self, idx: usize) -> &Object {
        self.heap.get(idx).unwrap()
    }

    fn add_string_intern(&mut self, string: &String) -> usize {
        match self.string_intern_pool.get(string).copied() {
            Some(idx) => idx,
            None => {
                let idx = self.heap.len();
                self.string_intern_pool.insert(string.to_owned(), idx);
                self.heap.push(Object::String(string.to_owned()));
                idx
            }
        }
    }
}
