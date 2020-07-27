use super::ast::{Atom, Stmt};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub type Spaghetti = Rc<RefCell<Option<Meatball>>>;

#[derive(Debug)]
pub struct Meatball {
    parent: Spaghetti,
    local: HashMap<String, Object>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    /// Atomic value
    Atom(Atom),
    /// Function arity, block
    Function(Vec<String>, Stmt),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Atom(a) => write!(f, "{}", a),
            Object::Function(_, _) => write!(f, "<function>"),
        }
    }
}

pub trait Parental {
    fn new_instance() -> Spaghetti;
    fn child(&self) -> Spaghetti;
    fn declare(&self, id: String, val: Object);
    fn update(&self, id: String, val: Object);
    fn get(&self, id: &str) -> Option<Object>;
}

impl Meatball {
    pub fn new() -> Meatball {
        Meatball {
            parent: Rc::new(RefCell::new(None)),
            local: HashMap::new(),
        }
    }

    /// Locally insert new Object
    pub fn insert(&mut self, name: String, value: Object) {
        self.local.insert(name, value);
    }

    /// Updates locally, or cascades up to ancestors to update at the first one found
    pub fn update_cascade(&mut self, name: String, value: Object) {
        if let Some(obj) = self.local.get_mut(&name).as_deref_mut() {
            *obj = value;
        } else {
            if let Some(parent) = self.parent.borrow_mut().as_mut() {
                parent.update_cascade(name, value);
            }
        }
    }
 
    /// Searches local map or searches through ancestors
    pub fn find(&self, name: &str) -> Option<Object> {
        if let Some(obj) = self.local.get(name).cloned() {
            Some(obj)
        } else {
            if let Some(parent) = self.parent.borrow().as_ref() {
                parent.find(name)
            } else {
                None
            }
        }
    }
}

impl Parental for Spaghetti {
    fn new_instance() -> Spaghetti {
        Spaghetti::new(RefCell::new(Some(Meatball::new())))
    }
    fn child(&self) -> Spaghetti {
        Rc::new(RefCell::new(Some(Meatball {
            local: HashMap::new(),
            parent: self.clone(),
        })))
    }
    fn declare(&self, id: String, val: Object) {
        self.borrow_mut().as_mut().unwrap().insert(id, val)
    }
    fn update(&self, id: String, val: Object) {
        self.borrow_mut().as_mut().unwrap().update_cascade(id, val)
    }
    fn get(&self, id: &str) -> Option<Object> {
        if let Some(meat) = self.borrow().as_ref() {
            meat.find(id)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_creation_and_fetch_var_from_parent() {
        let meatball = Spaghetti::new_instance();
        meatball.declare("var1".to_string(), Object::Atom(Atom::Boolean(true)));

        let childmeat = meatball.child();
        let parent_val = childmeat.get("var1").unwrap();
        assert_eq!(parent_val, Object::Atom(Atom::Boolean(true)));
    }

    #[test]
    fn test_updating_globally() {
        let meatball = Spaghetti::new_instance();
        meatball.declare("var1".to_string(), Object::Atom(Atom::Boolean(true)));

        let child = meatball.child();
        child.update("var1".to_string(), Object::Atom(Atom::Boolean(false)));
        assert_eq!(
            meatball.get("var1").unwrap(),
            Object::Atom(Atom::Boolean(false))
        );
    }
}
