use std::cmp;
use std::cell::RefCell;
use std::collections::HashMap;
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

// implements PartialEq, Debug
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
                parents: (omitted), children: {:?}, \
                table: (omitted) }}",
               self.identifier,
               self.states,
               self.children
                   .borrow()
                   .iter()
                   .map(|c| &c.identifier)
                   .collect::<Vec<_>>())
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

    pub fn link(&self, parent_identifier: &str, child_identifier: &str) {
        let parent = self.get_variable(parent_identifier)
                         .expect("can't link variable absent from network");
        let child = self.get_variable(child_identifier)
                        .expect("can't link variable absent from network");
        parent.children.borrow_mut().push(child.clone());
        child.parents.borrow_mut().push(Rc::downgrade(&parent));
    }

    fn collect_paths(at: Rc<Variable>, to: Rc<Variable>,
        mut journey: Vec<Rc<Variable>>, paths: &mut Vec<Path>) {
        for step in journey.iter() {
            if at == *step {
                // going in circles ...
                return;
            }
        }

        journey.push(at.clone());

        if at == to {
            // found it!
            paths.push(Path(journey.clone()));
            return;
        }

        for child in at.children.borrow().iter() {
            Network::collect_paths(child.clone(),
                                   to.clone(),
                                   journey.clone(),
                                   paths);
        }
        for weak_parent in at.parents.borrow().iter() {
            let parent = weak_parent.upgrade().unwrap();
            Network::collect_paths(parent.clone(),
                                   to.clone(),
                                   journey.clone(),
                                   paths);
        }

    }

    pub fn paths(&self, start_identifier: &str, end_identifier: &str)
                 -> Vec<Path> {
        let mut paths = Vec::new();
        let start = self.get_variable(start_identifier).unwrap();
        let end = self.get_variable(end_identifier).unwrap();
        Network::collect_paths(start, end, vec![], &mut paths);
        paths
    }

    pub fn d_separated(&self, start_identifier: &str, end_identifier: &str,
        given_identifiers: &[&str])
                       -> bool {
        let givens = given_identifiers.iter()
                                      .map(|&i| self.get_variable(i).unwrap())
                                      .collect::<Vec<_>>();
        self.paths(start_identifier, end_identifier)
            .iter()
            .all(|p| p.d_separated(&givens))
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

    fn center_in(&self, givens: &[Rc<Variable>]) -> bool {
        givens.contains(&self.nodes.1)
    }

    fn center_descendant_in(&self, givens: &[Rc<Variable>]) -> bool {
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
                    if segment.center_in(givens) {
                        return true;
                    }
                }
                SegmentTopology::Collider => {
                    if !segment.center_in(givens) &&
                       !segment.center_descendant_in(givens) {
                        return true;
                    }
                }
            }
        }
        false
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (i, window) in self.0.windows(2).enumerate() {
            let ref one = window[0];
            let ref other = window[1];
            let arrow = if one.children.borrow().contains(&other) {
                "→" // \u{2192} RIGHTWARDS ARROW
            } else {
                "←" // \u{2190} LEFTWARDS ARROW
            };

            if i == 0 {
                try!(write!(f,
                            "{} {} {}",
                            one.identifier,
                            arrow,
                            other.identifier));
            } else {
                try!(write!(f, " {} {}", arrow, other.identifier));
            }

        }
        Ok(())
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
        network.link("season", "sprinkler");
        network.link("season", "rain");
        network.link("sprinkler", "wet");
        network.link("rain", "wet");
        network.link("wet", "slippery");

        network
    }

    #[test]
    fn concerning_getting_a_variable() {
        let network = rain_sprinker_example();
        let rain = network.get_variable("rain").unwrap();
        assert_eq!("rain", rain.identifier);
    }

    #[test]
    fn concerning_paths() {
        let network = rain_sprinker_example();
        let paths = network.paths("season", "slippery");
        assert_eq!(2, paths.len());
        assert_eq!("season → sprinkler → wet → slippery",
                   format!("{}", paths[0]));
        assert_eq!("season → rain → wet → slippery",
                   format!("{}", paths[1]));
        let counterpaths = network.paths("slippery", "season");
        assert_eq!("slippery ← wet ← sprinkler ← season",
                   format!("{}", counterpaths[0]));
        assert_eq!("slippery ← wet ← rain ← season",
                   format!("{}", counterpaths[1]));
        assert_eq!(2, counterpaths.len());
    }

    #[test]
    fn concerning_d_separation() {
        let network = rain_sprinker_example();
        // _Causality_ §1.2.3
        //
        // "In Figure 1.2, X = {X₂} and Y = {X₃} are d-separated by Z = {X₁},
        // because both paths connecting {X₂} and {X₃} are blocked by Z. The
        // path X₂ ← X₁ → X₃ is blocked because it is a fork in which the
        // middle node X₁ is in Z, while the path X₂ → X₄ ← X₃ is blocked
        // because it is an inverted fork in which the middle node X₄ and all
        // its descendants are outside Z."
        assert!(network.d_separated("rain", "sprinkler", &["season"]));
        // "However, X and Y are not d-separated by the the set Z′, since X₅,
        // as descendant of the middle node X₄, is in Z′. Metaphorically,
        // learning the value of the consequence X₅ renders its causes X₂ and
        // X₃ dependent, as if a pathway were opened along the arrows
        // converging at X₄."
        assert!(!network.d_separated("rain",
                                     "sprinkler",
                                     &["season", "slippery"]));
    }

    #[test]
    fn concerning_linkage() {
        let network = rain_sprinker_example();
        let season = network.get_variable("season").unwrap();
        assert_eq!(vec!["sprinkler", "rain"],
                   season.children
                         .borrow()
                         .iter()
                         .map(|v| &v.identifier)
                         .collect::<Vec<_>>());
        for child in season.children.borrow().iter() {
            assert_eq!("wet", &child.children.borrow()[0].identifier);
            assert_eq!("season",
                       &child.parents.borrow()[0]
                            .upgrade()
                            .unwrap()
                            .identifier);
        }
    }

    #[test]
    fn concerning_descendants() {
        let network = rain_sprinker_example();
        let rain = network.get_variable("rain").unwrap();
        let rain_descendants = rain.descendants();
        assert_eq!(vec!["wet", "slippery"],
                   rain_descendants.iter()
                                   .map(|v| &v.identifier)
                                   .collect::<Vec<_>>());
    }

}
