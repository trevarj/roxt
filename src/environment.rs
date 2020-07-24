use super::ast::Atom;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type Spaghetti = Rc<RefCell<Option<Meatball>>>;

#[derive(Clone, Debug)]
pub struct Meatball {
    parent: Spaghetti,
    local: HashMap<String, Atom>,
}

pub trait Parental {
    fn new_instance() -> Spaghetti;
    fn child(&self) -> Spaghetti;
    fn set_var(&self, id: String, val: Atom);
    fn get_var(&self, id: String) -> Option<Atom>;
}

impl Meatball {
    pub fn new() -> Meatball {
        Meatball {
            parent: Rc::new(RefCell::new(None)),
            local: HashMap::new(),
        }
    }

    pub fn local_var(&mut self, name: String, value: Atom) {
        self.local.insert(name, value);
    }

    /// Searches local map or searches through ancestors
    pub fn find_var(&self, name: String) -> Option<Atom> {
        let mut var = self.local.get(&name).cloned();
        if var.is_none() {
            if let Some(parent) = self.parent.borrow().as_ref() {
                var = parent.find_var(name);
            }
        }
        var
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
    fn set_var(&self, id: String, val: Atom) {
        self.borrow_mut().as_mut().unwrap().local_var(id, val)
    }
    fn get_var(&self, id: String) -> Option<Atom> {
        if let Some(meat) = self.borrow().as_ref() {
            meat.find_var(id)
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
        meatball.set_var("var1".to_string(), Atom::Boolean(true));

        let childmeat = meatball.child();
        let parent_val = childmeat.get_var("var1".to_string()).unwrap();
        assert_eq!(parent_val, Atom::Boolean(true));
    }
}
