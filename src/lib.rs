use std::rc::Rc;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};


type Node = Rc<RefCell<Variable>>;

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    identifier: String,
    // Each variable should be able to have its own set of possible states. For
    // the moment, just use an opaque byte, because we should get the basics
    // down before we take on the struggle of wrangling trait objects.
    states: Vec<u8>,
    parents: Vec<Node>,
    children: Vec<Node>,
    // The conditional probability table is represented as a map of a vector of
    // parent states to probabilities.
    table: HashMap<Vec<u8>, Vec<f64>>,
}

impl Variable {
    fn new(identifier: &str) -> Node {
        Rc::new(RefCell::new(Variable {
            identifier: identifier.to_owned(),
            states: Vec::new(),
            parents: Vec::new(),
            children: Vec::new(),
            table: HashMap::new(),
        }))
    }

    fn paths_to(&self, other: Node) -> Vec<Path> {
        vec![Path(Vec::new())] // TODO!
    }

    fn collect_descendants(&self, descendants: &mut Vec<Node>) {
        for child in &self.children {
            if !descendants.contains(child) {
                descendants.push(child.clone());
                child.borrow().collect_descendants(descendants);
            }
        }
    }

    pub fn descendants(&self) -> Vec<Node> {
        let mut descendants = Vec::new();
        self.collect_descendants(&mut descendants);
        descendants
    }

    pub fn d_separated_from(&self, other: Node, givens: &[Node]) -> bool {
        let paths = self.paths_to(other);
        paths.iter().any(|ref p| p.d_separated(givens))
    }
}

#[derive(Clone)]
struct Network(Vec<Node>);

impl Network {
    pub fn new(nodes: &[Node]) -> Self {
        Network(nodes.iter().cloned().collect::<Vec<Node>>())
    }

    pub fn get_variable(&self, identifier: &str) -> Option<&Node> {
        self.0
            .iter()
            .skip_while(|&n| n.borrow().identifier != identifier)
            .next()
    }
}

struct Path(Vec<Node>);

#[derive(Clone, Copy, Debug)]
enum SegmentTopology {
    Chain,
    Fork,
    Collider,
}

#[derive(Clone, Debug)]
struct Segment {
    nodes: (Node, Node, Node),
    topology: SegmentTopology,
}

impl Segment {
    fn center_in_givens(&self, givens: &[Node]) -> bool {
        givens.contains(&self.nodes.1)
    }

    fn center_descendant_in_givens(&self, givens: &[Node]) -> bool {
        for descendant in self.nodes.1.borrow().descendants() {
            if givens.contains(&descendant) {
                return true;
            }
        }
        false
    }
}

impl From<Path> for Segment {
    fn from(path: Path) -> Self {
        if path.0.len() != 3 {
            panic!("a segment must have exactly three nodes");
        }

        if path.0[0].borrow().children.contains(&path.0[1]) {
            if path.0[1].borrow().children.contains(&path.0[2]) {
                Segment {
                    nodes: (path.0[0].clone(),
                            path.0[1].clone(),
                            path.0[2].clone()),
                    topology: SegmentTopology::Chain,
                }
            } else if path.0[1].borrow().parents.contains(&path.0[2]) {
                Segment {
                    nodes: (path.0[0].clone(),
                            path.0[1].clone(),
                            path.0[2].clone()),
                    topology: SegmentTopology::Collider,
                }
            } else {
                panic!("expected parent-child relation for adjacent segment \
                        nodes");
            }
        } else if path.0[0].borrow().parents.contains(&path.0[1]) {
            if path.0[1].borrow().children.contains(&path.0[2]) {
                Segment {
                    nodes: (path.0[0].clone(),
                            path.0[1].clone(),
                            path.0[2].clone()),
                    topology: SegmentTopology::Fork,
                }
            } else if path.0[1].borrow().parents.contains(&path.0[2]) {
                Segment {
                    nodes: (path.0[0].clone(),
                            path.0[1].clone(),
                            path.0[2].clone()),
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
}

impl Path {
    fn d_separated(&self, givens: &[Node]) -> bool {
        for window in self.0.windows(3) {
            let segment = Segment::from(Path(window.to_vec()));
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
        let season = Variable::new("season");
        let sprinkler = Variable::new("sprinkler");
        let rain = Variable::new("rain");
        let wet = Variable::new("wet");
        let slippery = Variable::new("slippery");
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
        assert_eq!("rain", rain.borrow().identifier);
    }

    #[test]
    fn concerning_tests_that_have_yet_to_be_written() {}

}
