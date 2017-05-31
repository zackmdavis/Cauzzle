#[macro_use] extern crate log;
extern crate petgraph;
extern crate rand;

use std::collections::HashMap;
use std::fmt;

use petgraph::{Directed, Direction, Graph};
use petgraph::graph::{NodeIndex, EdgeIndex};
use petgraph::visit::Topo;


#[derive(Copy, Clone, Debug)]
enum VariableState {
    Categorical(usize),
    Integer(isize),
    Float(f64)
}

// implements Debug
struct Variable {
    identifier: String,
    state: Option<VariableState>,
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
struct StructuralCausalModel(Graph<Variable, (), Directed>);

impl StructuralCausalModel {
    fn new() -> Self {
        StructuralCausalModel(Graph::new())
    }

    fn add_variable(&mut self, variable: Variable) -> NodeIndex {
        self.0.add_node(variable)
    }

    fn add_arrow(&mut self,
                 parent_index: NodeIndex,
                 child_index: NodeIndex) -> EdgeIndex {
        self.0.add_edge(parent_index, child_index, ())
    }

    fn parent_states(&self, index: NodeIndex) -> Vec<VariableState> {
        self.0.neighbors_directed(index, Direction::Incoming)
            .map(|j| self.0.node_weight(j).expect("parent exists")
                 .state.expect("parent state must be set"))
            .collect()
    }

    fn evaluate_variable(&mut self, index: NodeIndex) {
        let parent_states = self.parent_states(index);
        let state = (self.0.node_weight(index)
                     .expect("variable should exist").structure)(&parent_states);
        info!("setting variable {:?} state to {:?} based on parent states {:?}",
              index, state, parent_states);
        self.0.node_weight_mut(index)
            .expect("variable should exist").state = Some(state);
    }

    fn evaluate(&mut self) {
        let mut topological_visitor = Topo::new(&self.0);
        while let Some(next_index) = topological_visitor.next(&self.0) {
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
        let one_penny_index = model.add_variable(one_penny);
        let another_penny_index = model.add_variable(another_penny);
        let matching_index = model.add_variable(matching);
        model.add_arrow(one_penny_index, matching_index);
        model.add_arrow(another_penny_index, matching_index);

        model.evaluate();
    }
}
