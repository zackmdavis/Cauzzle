use std::rc::Rc;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    identifier: String,
    // Each variable should be able to have its own set of possible states. For
    // the moment, just use an opaque byte, because we should get the basics
    // down before we take on the struggle of wrangling trait objects.
    states: Vec<u8>,
    parents: Vec<Rc<RefCell<Variable>>>,
    children: Vec<Rc<RefCell<Variable>>>,
    // The conditional probability table is represented as a map of a vector of
    // parent states to probabilities.
    table: HashMap<Vec<u8>, Vec<f64>>,
}

impl Variable {
    fn new(identifier: &str) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Variable {
            identifier: identifier.to_owned(),
            states: Vec::new(),
            parents: Vec::new(),
            children: Vec::new(),
            table: HashMap::new(),
        }))
    }

    fn paths_to(&self, other: Rc<RefCell<Variable>>) -> Vec<Path> {
        vec![Path(Vec::new())] // TODO!
    }

    fn collect_descendants(&self, descendants: &mut Vec<Rc<RefCell<Variable>>>) {
        for child in &self.children {
            if !descendants.contains(child) {
                descendants.push(child.clone());
                child.borrow().collect_descendants(descendants);
            }
        }
    }

    pub fn descendants(&self) -> Vec<Rc<RefCell<Variable>>> {
        let mut descendants = Vec::new();
        self.collect_descendants(&mut descendants);
        descendants
    }

    fn d_separated_from(&self, other: Rc<RefCell<Variable>>,
        givens: &[Rc<RefCell<Variable>>])
                        -> bool {
        let paths = self.paths_to(other);
        paths.iter().any(|ref p| p.d_separated(givens))
    }
}

struct Path(Vec<Rc<RefCell<Variable>>>);


enum Segment {
    Chain(Rc<RefCell<Variable>>, Rc<RefCell<Variable>>, Rc<RefCell<Variable>>),
    Fork(Rc<RefCell<Variable>>, Rc<RefCell<Variable>>, Rc<RefCell<Variable>>),
    Collider(Rc<RefCell<Variable>>,
             Rc<RefCell<Variable>>,
             Rc<RefCell<Variable>>),
}

impl From<Path> for Segment {
    fn from(path: Path) -> Self {
        if path.0.len() != 3 {
            panic!("a segment must have exactly three nodes");
        }

        if path.0[0].borrow().children.contains(&path.0[1]) {
            if path.0[1].borrow().children.contains(&path.0[2]) {
                Segment::Chain(path.0[0].clone(),
                               path.0[1].clone(),
                               path.0[2].clone())
            } else if path.0[1].borrow().parents.contains(&path.0[2]) {
                Segment::Collider(path.0[0].clone(),
                                  path.0[1].clone(),
                                  path.0[2].clone())
            } else {
                panic!("expected parent-child relation for adjacent segment \
                        nodes");
            }
        } else if path.0[0].borrow().parents.contains(&path.0[1]) {
            if path.0[1].borrow().children.contains(&path.0[2]) {
                Segment::Fork(path.0[0].clone(),
                              path.0[1].clone(),
                              path.0[2].clone())
            } else if path.0[1].borrow().parents.contains(&path.0[2]) {
                Segment::Chain(path.0[0].clone(),
                               path.0[1].clone(),
                               path.0[2].clone())

            } else {
                panic!("expected parent-child relation for adjacent segment \
                        nodes");
            }
        } else {
            panic!("expected parent-child relation for adjacent segment \
                    nodes");
        }

    }
}

impl Path {
    fn d_separated(&self, givens: &[Rc<RefCell<Variable>>]) -> bool {
        for window in self.0.windows(3) {
            let segment = Segment::from(Path(window.to_vec()));
            // TODO
        }
        false // TODO
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn concerning_tests_that_have_yet_to_be_written() {}
}
