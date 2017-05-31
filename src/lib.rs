#![allow(dead_code)]

#[macro_use] extern crate log;
extern crate petgraph;
extern crate rand;

use std::collections::HashMap;
use std::fmt;

use petgraph::{Directed, Direction, Graph};
use petgraph::graph::NodeIndex;
use petgraph::visit::Topo;


#[derive(Copy, Clone, Debug)]
enum VariableState {
    Categorical(usize),
    Integer(isize),
    Float(f64)
}

impl VariableState {
    fn categorical(&self) -> usize {
        match *self {
            VariableState::Categorical(c) => c,
            s @ _ => panic!("expected variable state to be categorical, \
                             got {:?}", s)
        }
    }

    fn integer(&self) -> isize {
        match *self {
            VariableState::Integer(i) => i,
            s @ _ => panic!("expected variable state to be integral, \
                             got {:?}", s)
        }
    }

    fn float(&self) -> f64 {
        match *self {
            VariableState::Float(f) => f,
            s @ _ => panic!("expected variable state to be a float, \
                             got {:?}", s)
        }
    }
}

// implements Debug
struct Variable {
    identifier: String,
    state: Option<VariableState>,
    // XXX: the argument is going to have to be &[(String, VariableState)] to
    // disambiguate between parents (can't depend on graph order)
    structure: Box<Fn(&[VariableState]) -> VariableState>
}

impl Variable {
    fn new<I: ToString>(identifier: I, state: Option<VariableState>,
                        structure: Box<Fn(&[VariableState]) -> VariableState>)
                        -> Self {
        Variable { identifier: identifier.to_string(),
                   state: state,
                   structure: structure }
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f,
               "Variable {{ identifier: {:?}, state: {:?}, structure: @{:p} }}",
               self.identifier, self.state, &self.structure)
    }
}

#[derive(Debug)]
struct StructuralCausalModel {
    identifiers: HashMap<String, NodeIndex>,
    graph: Graph<Variable, (), Directed>,
}

impl StructuralCausalModel {
    fn new() -> Self {
        StructuralCausalModel {
            identifiers: HashMap::new(),
            graph: Graph::new()
        }
    }

    pub fn add_variable(&mut self, variable: Variable) {
        let identifier = variable.identifier.clone();
        let index = self.graph.add_node(variable);
        self.identifiers.insert(identifier, index);
    }

    pub fn get_variable(&self, identifier: &str) -> Option<&Variable> {
        self.graph.node_weight(self.identifiers[identifier])
    }

    pub fn add_arrow(&mut self, parent: &str, child: &str) {
        self.graph.add_edge(self.identifiers[parent],
                            self.identifiers[child], ());
    }

    fn parent_states(&self, index: NodeIndex) -> Vec<VariableState> {
        self.graph.neighbors_directed(index, Direction::Incoming)
            .map(|j| self.graph.node_weight(j).expect("parent exists")
                 .state.expect("parent state must be set"))
            .collect()
    }

    fn evaluate_variable(&mut self, index: NodeIndex) {
        let parent_states = self.parent_states(index);
        let state = (self.graph.node_weight(index)
                     .expect("variable should exist").structure)(&parent_states);
        info!("setting variable {:?} state to {:?} based on parent states {:?}",
              index, state, parent_states);
        self.graph.node_weight_mut(index)
            .expect("variable should exist").state = Some(state);
    }

    pub fn evaluate(&mut self) {
        let mut topological_visitor = Topo::new(&self.graph);
        while let Some(next_index) = topological_visitor.next(&self.graph) {
            self.evaluate_variable(next_index);
        }
    }
}

#[cfg(test)]
mod tests {
    use rand::random;

    use super::{StructuralCausalModel, Variable, VariableState};


    const HEADS: VariableState = VariableState::Categorical(0);
    const TAILS: VariableState = VariableState::Categorical(1);

    fn coinflip(_dummy: &[VariableState]) -> VariableState {
        if random::<f64>() < 0.5 {
            HEADS
        } else {
            TAILS
        }
    }

    fn parity(parents: &[VariableState]) -> VariableState {
        let mut parity_bit = 0;
        for parent in parents {
            match parent {
                &VariableState::Categorical(i) => {
                    parity_bit = (parity_bit + i) % 2;
                }
                s @ _ => panic!("unexpectedly non-categorical \
                                 variable state {:?}", s)
            };
        }
        VariableState::Categorical(parity_bit)
    }

    #[test]
    fn matching_pennies() {
        // Let's start with a minimal example. Say, a collider: two coinflips
        // jointly cause a variable indicating their parity.
        let one_penny = Variable::new("penny A", None, Box::new(coinflip));
        let another_penny = Variable::new("penny B", None, Box::new(coinflip));
        let matching = Variable::new("matching", None, Box::new(parity));

        // set up the graph
        let mut model = StructuralCausalModel::new();
        model.add_variable(one_penny);
        model.add_variable(another_penny);
        model.add_variable(matching);
        model.add_arrow("penny A", "matching");
        model.add_arrow("penny B", "matching");

        model.evaluate();

        let first_flip = model.get_variable("penny A")
            .expect("variable should exist")
            .state.expect("state should be set").categorical();
        let second_flip = model.get_variable("penny B")
            .expect("variable should exist")
            .state.expect("state should be set").categorical();
        let parity_result = model.get_variable("matching")
            .expect("variable should exist")
            .state.expect("state should be set").categorical();

        assert_eq!(parity_result, (first_flip + second_flip) % 2);
    }
}
