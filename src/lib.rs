use std::cmp;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem;
use std::rc::{Rc, Weak};


struct Variable {
    identifier: String,
    // Each variable should be able to have its own set of possible states. For
    // the moment, just use an opaque byte, because we should get the basics
    // down before we take on the struggle of wrangling trait objects.
    states: Vec<u8>,
    parents: RefCell<Vec<Weak<Variable>>>,
    children: RefCell<Vec<Rc<Variable>>>,
    // The conditional probability table is represented as a map of a vector of
    // parent states to probabilities.
    table: RefCell<HashMap<Vec<u8>, Vec<f64>>>,
}

impl Variable {
    fn create(identifier: &str, states: &[u8]) -> Rc<Variable> {
        Rc::new(Variable {
            identifier: identifier.to_owned(),
            states: states.to_vec(),
            parents: RefCell::new(Vec::new()),
            children: RefCell::new(Vec::new()),
            table: RefCell::new(HashMap::new()),
        })
    }

    fn paths_to(&self, other: Rc<Variable>) -> Vec<Path> {
        vec![Path(Vec::new())] // TODO!
    }

    fn collect_descendants(&self, descendants: &mut Vec<Rc<Variable>>) {
        for child in self.children.borrow().iter() {
            if !descendants.contains(child) {
                descendants.push(child.clone());
                child.collect_descendants(descendants);
            }
        }
    }

    pub fn descendants(&self) -> Vec<Rc<Variable>> {
        let mut descendants = Vec::new();
        self.collect_descendants(&mut descendants);
        descendants
    }

    pub fn d_separated_from(&self, other: Rc<Variable>,
        givens: &[Rc<Variable>])
                            -> bool {
        let paths = self.paths_to(other);
        paths.iter().any(|ref p| p.d_separated(givens))
    }
}

impl cmp::PartialEq for Variable {
    fn eq(&self, other: &Variable) -> bool {
        self.identifier == other.identifier
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f,
               "Variable {{ identifier: {:?}, states: {:?}, \
                parents: (omitted), children: (omitted), \
                table: (omitted) }}",
               self.identifier,
               self.states)
    }
}


#[derive(Clone, Debug)]
struct Network(Vec<Rc<Variable>>);

impl Network {
    pub fn new(nodes: &[Rc<Variable>]) -> Self {
        Network(nodes.iter().cloned().collect::<Vec<Rc<Variable>>>())
    }

    pub fn get_variable(&self, identifier: &str) -> Option<Rc<Variable>> {
        self.0
            .iter()
            .skip_while(|&n| n.identifier != identifier)
            .next()
            .cloned()
    }
}

#[derive(Clone, Copy, Debug)]
enum SegmentTopology {
    Chain,
    Fork,
    Collider,
}

#[derive(Clone, Debug)]
struct Segment {
    nodes: (Rc<Variable>, Rc<Variable>, Rc<Variable>),
    topology: SegmentTopology,
}

impl Segment {
    fn new(subpath: &[Rc<Variable>]) -> Self {
        if subpath.len() != 3 {
            panic!("a segment must have exactly three nodes");
        }

        if subpath[0].children.borrow().contains(&subpath[1]) {
            if subpath[1].children.borrow().contains(&subpath[2]) {
                Segment {
                    nodes: (subpath[0].clone(),
                            subpath[1].clone(),
                            subpath[2].clone()),
                    topology: SegmentTopology::Chain,
                }
            } else if subpath[2].children.borrow().contains(&subpath[1]) {
                Segment {
                    nodes: (subpath[0].clone(),
                            subpath[1].clone(),
                            subpath[2].clone()),
                    topology: SegmentTopology::Collider,
                }
            } else {
                panic!("expected parent-child relation for adjacent segment \
                        nodes");
            }
        } else if subpath[1].children.borrow().contains(&subpath[0]) {
            if subpath[1].children.borrow().contains(&subpath[2]) {
                Segment {
                    nodes: (subpath[0].clone(),
                            subpath[1].clone(),
                            subpath[2].clone()),
                    topology: SegmentTopology::Fork,
                }
            } else if subpath[2].children.borrow().contains(&subpath[1]) {
                Segment {
                    nodes: (subpath[0].clone(),
                            subpath[1].clone(),
                            subpath[2].clone()),
                    topology: SegmentTopology::Chain,
                }

            } else {
                panic!("expected parent-child relation for adjacent segment \
                        nodes");
            }
        } else {
            panic!("expected parent-child relation for adjacent segment \
                    nodes");
        }

    }

    fn center_in_givens(&self, givens: &[Rc<Variable>]) -> bool {
        givens.contains(&self.nodes.1)
    }

    fn center_descendant_in_givens(&self, givens: &[Rc<Variable>]) -> bool {
        for descendant in self.nodes.1.descendants() {
            if givens.contains(&descendant) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug)]
struct Path(Vec<Rc<Variable>>);

impl Path {
    fn d_separated(&self, givens: &[Rc<Variable>]) -> bool {
        for window in self.0.windows(3) {
            let segment = Segment::new(window);
            match segment.topology {
                SegmentTopology::Chain | SegmentTopology::Fork => {
                    if segment.center_in_givens(givens) {
                        return true;
                    }
                }
                SegmentTopology::Collider => {
                    if !segment.center_in_givens(givens) &&
                       !segment.center_descendant_in_givens(givens) {
                        return true;
                    }
                }
            }
        }
        false
    }
}

#[cfg(test)]
mod test {
    use super::{Network, Variable};

    // the example network from _Causality_ §1.2
    fn rain_sprinker_example() -> Network {
        let season = Variable::create("season", &[1, 2, 3, 4]);
        let sprinkler = Variable::create("sprinkler", &[0, 1]);
        let rain = Variable::create("rain", &[1, 2, 3]);
        let wet = Variable::create("wet", &[1, 2, 3]);
        let slippery = Variable::create("slippery", &[1, 2, 3]);
        let nodes = vec![season.clone(),
                         rain.clone(),
                         sprinkler.clone(),
                         wet.clone(),
                         slippery.clone()];
        let network = Network::new(&nodes);
        network
    }

    #[test]
    fn concerning_getting_a_variable() {
        let network = rain_sprinker_example();
        let rain = network.get_variable("rain").unwrap();
        assert_eq!("rain", rain.identifier);
    }

    #[test]
    fn concerning_tests_that_have_yet_to_be_written() {}

}
