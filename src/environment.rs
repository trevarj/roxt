use super::ast::{Atom, Stmt};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub type Spaghetti = Rc<RefCell<Meatball>>;

#[derive(Debug, PartialEq)]
pub struct Meatball {
    id: usize,
    parent: Option<Spaghetti>,
    local: HashMap<String, Object>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    /// Atomic value
    Atom(Atom),
    /// Function arity, block, closure env
    Function(Vec<String>, Stmt, Spaghetti),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Atom(a) => write!(f, "{}", a),
            Object::Function(_, _, _) => write!(f, "<function>"),
        }
    }
}

pub trait Parental {
    fn new_instance() -> Spaghetti;
    fn new_scope(&self) -> Spaghetti;
    fn assign(&self, id: String, val: Object);
    fn assign_at_distance(&self, id: String, val: Object, distance: usize);
    fn get(&self, id: &str) -> Option<Object>;
    fn get_by_distance(&self, id: &str, distance: usize) -> Option<Object>;
}

impl Meatball {
    pub fn new() -> Meatball {
        Meatball {
            id: 0,
            parent: None,
            local: HashMap::new(),
        }
    }

    pub fn get_id(&self) -> usize {
        self.id
    }

    /// Locally insert new Object
    pub fn assign(&mut self, name: String, value: Object) {
        println!("id {}, assigning {} {} ", self.id, name, value);
        self.local.insert(name, value);
    }

    pub fn assign_at_distance(&mut self, name: String, value: Object, distance: usize) {
        println!(
            "id {}, assigning {} to {} at distance {}",
            self.id, name, value, distance
        );
        if distance == 0 {
            self.assign(name, value)
        } else {
            if let Some(p) = self.parent.as_mut() {
                p.borrow_mut().assign_at_distance(name, value, distance - 1)
            } else {
                // throw error
            }
        }
    }

    /// Searches local map or searches through ancestors
    pub fn get(&self, name: &str) -> Option<Object> {
        println!("id {}, getting {} ", self.id, name);
        self.local.get(name).cloned()
    }

    /// Searches local map or searches through ancestors
    pub fn get_by_distance(&self, name: &str, distance: usize) -> Option<Object> {
        println!("id {}, getting {} at distance {}", self.id, name, distance);
        if distance == 0 {
            self.get(name)
        } else {
            if let Some(p) = self.parent.as_ref() {
                p.get_by_distance(name, distance - 1)
            } else {
                None
            }
        }
    }
}

impl Parental for Spaghetti {
    fn new_instance() -> Spaghetti {
        Spaghetti::new(RefCell::new(Meatball::new()))
    }
    fn new_scope(&self) -> Spaghetti {
        let parent_id = self.borrow().get_id();
        println!("creating new scope id {}", parent_id + 1);
        Rc::new(RefCell::new(Meatball {
            id: parent_id + 1,
            local: HashMap::new(),
            parent: Some(self.clone()),
        }))
    }
    fn assign(&self, id: String, val: Object) {
        self.borrow_mut().assign(id, val)
    }
    fn assign_at_distance(&self, id: String, val: Object, distance: usize) {
        self.borrow_mut().assign_at_distance(id, val, distance)
    }
    fn get(&self, id: &str) -> Option<Object> {
        self.borrow().get(id)
    }
    fn get_by_distance(&self, id: &str, distance: usize) -> Option<Object> {
        self.borrow().get_by_distance(id, distance)
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

// }
