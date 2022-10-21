use crate::parser::{NodeId, NodeType};

#[derive(Debug)]
pub struct ParserDelta {
    pub node_id_offset: usize,
    pub span_start: Vec<usize>,
    pub span_end: Vec<usize>,
    pub node_types: Vec<NodeType>,
}

impl ParserDelta {
    pub fn new(node_id_offset: usize) -> Self {
        Self {
            node_id_offset,
            span_start: vec![],
            span_end: vec![],
            node_types: vec![],
        }
    }

    pub fn print(&self) {
        if self.node_types.is_empty() {
            println!("<empty>");
        } else {
            self.print_helper(&NodeId(self.node_types.len() - 1), 0)
        }
    }

    fn print_helper(&self, node_id: &NodeId, indent: usize) {
        for _ in 0..indent {
            print!(" ")
        }

        match &self.node_types[node_id.0] {
            NodeType::Let {
                variable_name,
                initializer,
            } => {
                println!(
                    "Let ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::LetEnv {
                variable_name,
                initializer,
            } => {
                println!(
                    "LetEnv ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::Mut {
                variable_name,
                initializer,
            } => {
                println!(
                    "Mut ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(variable_name, indent + 2);
                self.print_helper(initializer, indent + 2);
            }
            NodeType::Param { name, ty } => {
                println!(
                    "Param ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2);
                }
            }
            NodeType::Def {
                name,
                params,
                block,
            } => {
                println!(
                    "Def ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                self.print_helper(block, indent + 2);
            }
            NodeType::DefEnv {
                name,
                params,
                block,
            } => {
                println!(
                    "DefEnv ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(name, indent + 2);
                self.print_helper(params, indent + 2);
                self.print_helper(block, indent + 2);
            }
            NodeType::Block(nodes) => {
                println!(
                    "Block ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            NodeType::Params(nodes) => {
                print!(
                    "Params ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                if nodes.is_empty() {
                    println!(" <empty>");
                } else {
                    println!();
                }

                for node in nodes {
                    self.print_helper(node, indent + 2);
                }
            }
            NodeType::Call { head, args } => {
                println!(
                    "Call ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(head, indent + 2);

                for arg in args {
                    self.print_helper(arg, indent + 2);
                }
            }
            NodeType::List(items) => {
                println!(
                    "List ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                for item in items {
                    self.print_helper(item, indent + 2);
                }
            }
            NodeType::Pipeline { from, to } => {
                println!(
                    "Pipeline ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(from, indent + 2);
                self.print_helper(to, indent + 2)
            }
            NodeType::Redirection { from, to } => {
                println!(
                    "Redirection ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(from, indent + 2);
                self.print_helper(to, indent + 2)
            }
            NodeType::BinaryOp { lhs, op, rhs } => {
                println!(
                    "BinaryOp ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(op, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::BashAnd { lhs, rhs } => {
                println!(
                    "BashAnd ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::BashOr { lhs, rhs } => {
                println!(
                    "BashOr ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );

                self.print_helper(lhs, indent + 2);
                self.print_helper(rhs, indent + 2)
            }
            NodeType::If {
                condition,
                then_block,
                else_expression,
            } => {
                println!(
                    "If ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
                self.print_helper(then_block, indent + 2);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2)
                }
            }
            NodeType::Where(condition) => {
                println!(
                    "Where ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                self.print_helper(condition, indent + 2);
            }
            x => {
                println!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0],
                )
            }
        }
    }
}
