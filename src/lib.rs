use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

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
    table: HashMap<Vec<u8>, Vec<f64>>
}

impl Variable {
    fn new(identifier: &str) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Variable {
            identifier: identifier.to_owned(),
            states: Vec::new(),
            parents: Vec::new(),
            children: Vec::new(),
            table: HashMap::new()
        }))
    }

    fn paths_to(other: Variable) -> Vec<Vec<Rc<RefCell<Variable>>>> {
        vec![Vec::new()] // TODO!
    }

    fn d_separated_from(other: Variable) -> bool {
        false // TODO!
    }

}

#[cfg(test)]
mod test {
    #[test]
    fn concerning_tests_that_have_yet_to_be_written() {}
}
