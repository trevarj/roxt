use crate::object::Object;

#[derive(Debug)]
pub struct Memory {
    heap: Vec<Object>
}

impl Memory {
    pub fn new() -> Memory {
        Memory {
            heap: Vec::with_capacity(512)
        }
    }

    /// Pushes object onto the heap and returns the index
    pub fn add_object(&mut self, obj: Object) -> usize {
        self.heap.push(obj);
        self.heap.len() - 1
    }

    pub fn get_object_pointer(&self, idx: usize) -> &Object {
        self.heap.get(idx).unwrap()
    }
}