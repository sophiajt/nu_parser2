#[derive(Debug)]
pub enum ShellErrorType {
    Expected(String),
}

#[derive(Debug)]
pub struct ShellError {
    pub error_type: ShellErrorType,
    pub span_start: usize,
    pub span_end: usize,
}
