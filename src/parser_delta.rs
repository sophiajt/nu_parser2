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

    pub fn print(&self, buffer: &mut Option<&mut String>) {
        if self.node_types.is_empty() {
            report_line(&format!("<empty>"), buffer);
        } else {
            self.print_helper(&NodeId(self.node_types.len() - 1), 0, buffer)
        }
    }

    fn print_helper(&self, node_id: &NodeId, indent: usize, buffer: &mut Option<&mut String>) {
        for _ in 0..indent {
            report(" ", buffer);
        }

        match &self.node_types[node_id.0] {
            NodeType::Let {
                variable_name,
                initializer,
            } => {
                report_line(
                    &format!(
                        "Let ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(variable_name, indent + 2, buffer);
                self.print_helper(initializer, indent + 2, buffer);
            }
            NodeType::LetEnv {
                variable_name,
                initializer,
            } => {
                report_line(
                    &format!(
                        "LetEnv ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(variable_name, indent + 2, buffer);
                self.print_helper(initializer, indent + 2, buffer);
            }
            NodeType::Mut {
                variable_name,
                initializer,
            } => {
                report_line(
                    &format!(
                        "Mut ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(variable_name, indent + 2, buffer);
                self.print_helper(initializer, indent + 2, buffer);
            }
            NodeType::Param { name, ty } => {
                report_line(
                    &format!(
                        "Param ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(name, indent + 2, buffer);
                if let Some(ty) = ty {
                    self.print_helper(ty, indent + 2, buffer);
                }
            }
            NodeType::Def {
                name,
                params,
                block,
            } => {
                report_line(
                    &format!(
                        "Def ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(name, indent + 2, buffer);
                self.print_helper(params, indent + 2, buffer);
                self.print_helper(block, indent + 2, buffer);
            }
            NodeType::DefEnv {
                name,
                params,
                block,
            } => {
                report_line(
                    &format!(
                        "DefEnv ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(name, indent + 2, buffer);
                self.print_helper(params, indent + 2, buffer);
                self.print_helper(block, indent + 2, buffer);
            }
            NodeType::Closure { params, block } => {
                report_line(
                    &format!(
                        "Closure ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(params, indent + 2, buffer);
                self.print_helper(block, indent + 2, buffer);
            }
            NodeType::Block(nodes) => {
                report_line(
                    &format!(
                        "Block ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                for node in nodes {
                    self.print_helper(node, indent + 2, buffer);
                }
            }
            NodeType::Params(nodes) => {
                print!(
                    "Params ({}, {}):",
                    self.span_start[node_id.0], self.span_end[node_id.0],
                );
                if nodes.is_empty() {
                    report_line(&format!(" <empty>"), buffer);
                } else {
                    report_line("", buffer);
                }

                for node in nodes {
                    self.print_helper(node, indent + 2, buffer);
                }
            }
            NodeType::Call { head, args } => {
                report_line(
                    &format!(
                        "Call ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(head, indent + 2, buffer);

                for arg in args {
                    self.print_helper(arg, indent + 2, buffer);
                }
            }
            NodeType::List(items) => {
                report_line(
                    &format!(
                        "List ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                for item in items {
                    self.print_helper(item, indent + 2, buffer);
                }
            }
            NodeType::Table(items) => {
                report_line(
                    &format!(
                        "Table ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                for item in items {
                    self.print_helper(item, indent + 2, buffer);
                }
            }
            NodeType::Record(items) => {
                report_line(
                    &format!(
                        "Record ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                for item in items {
                    self.print_helper(item, indent + 2, buffer);
                }
            }
            NodeType::RecordField { label, value } => {
                report_line(
                    &format!(
                        "Field ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(label, indent + 2, buffer);
                self.print_helper(value, indent + 2, buffer);
            }
            NodeType::Pipeline { from, to } => {
                report_line(
                    &format!(
                        "Pipeline ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(from, indent + 2, buffer);
                self.print_helper(to, indent + 2, buffer);
            }
            NodeType::Redirection { from, to } => {
                report_line(
                    &format!(
                        "Redirection ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(from, indent + 2, buffer);
                self.print_helper(to, indent + 2, buffer);
            }
            NodeType::BinaryOp { lhs, op, rhs } => {
                report_line(
                    &format!(
                        "BinaryOp ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(lhs, indent + 2, buffer);
                self.print_helper(op, indent + 2, buffer);
                self.print_helper(rhs, indent + 2, buffer);
            }
            NodeType::Range { lhs, rhs } => {
                report_line(
                    &format!(
                        "Range ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(lhs, indent + 2, buffer);
                self.print_helper(rhs, indent + 2, buffer);
            }
            NodeType::RowPath { head, path } => {
                report_line(
                    &format!(
                        "RowPath ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(head, indent + 2, buffer);
                self.print_helper(path, indent + 2, buffer);
            }
            NodeType::ColumnPath { head, path } => {
                report_line(
                    &format!(
                        "ColumnPath ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(head, indent + 2, buffer);
                self.print_helper(path, indent + 2, buffer);
            }
            NodeType::BashAnd { lhs, rhs } => {
                report_line(
                    &format!(
                        "BashAnd ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(lhs, indent + 2, buffer);
                self.print_helper(rhs, indent + 2, buffer);
            }
            NodeType::BashOr { lhs, rhs } => {
                report_line(
                    &format!(
                        "BashOr ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );

                self.print_helper(lhs, indent + 2, buffer);
                self.print_helper(rhs, indent + 2, buffer);
            }
            NodeType::If {
                condition,
                then_block,
                else_expression,
            } => {
                report_line(
                    &format!(
                        "If ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(condition, indent + 2, buffer);
                self.print_helper(then_block, indent + 2, buffer);
                if let Some(else_expression) = else_expression {
                    self.print_helper(else_expression, indent + 2, buffer)
                }
            }
            NodeType::Where(condition) => {
                report_line(
                    &format!(
                        "Where ({}, {}):",
                        self.span_start[node_id.0], self.span_end[node_id.0],
                    ),
                    buffer,
                );
                self.print_helper(condition, indent + 2, buffer);
            }
            x => report_line(
                &format!(
                    "{:?} ({}, {})",
                    x, self.span_start[node_id.0], self.span_end[node_id.0],
                ),
                buffer,
            ),
        }
    }
}

fn report(msg: &str, buffer: &mut Option<&mut String>) {
    if let Some(buf) = buffer {
        buf.push_str(msg);
    } else {
        print!("{}", msg);
    }
}

fn report_line(msg: &str, buffer: &mut Option<&mut String>) {
    if let Some(buf) = buffer {
        buf.push_str(msg);
        buf.push('\n');
    } else {
        println!("{}", msg);
    }
}
