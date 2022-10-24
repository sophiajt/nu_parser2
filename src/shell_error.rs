#[derive(Debug, Clone, Copy)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, Copy)]
enum MessageSeverity {
    Hint,
    Error,
}

impl MessageSeverity {
    pub fn name(&self) -> &str {
        match self {
            MessageSeverity::Hint => "Hint",
            MessageSeverity::Error => "Error",
        }
    }
    pub fn ansi_color_code(&self) -> &str {
        match self {
            MessageSeverity::Hint => "94",  // Bright Blue
            MessageSeverity::Error => "31", // Red
        }
    }
}

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

impl ShellError {
    pub fn print(&self, filename: &str, file_contents: &[u8]) {
        match &self.error_type {
            ShellErrorType::Expected(message) => display_message_with_span(
                MessageSeverity::Error,
                filename,
                file_contents,
                message,
                Span {
                    start: self.span_start,
                    end: self.span_end,
                },
            ),
        }
    }
}

fn display_message_with_span(
    severity: MessageSeverity,
    filename: &str,
    file_contents: &[u8],
    message: &str,
    span: Span,
) {
    eprintln!("{}: {}", severity.name(), message);

    let line_spans = gather_line_spans(file_contents);

    let mut line_index = 1;

    let mut largest_line_number = 0;
    while line_index < line_spans.len() {
        if span.start >= line_spans[line_index].0 && span.start <= line_spans[line_index].1 {
            largest_line_number = line_index + 2; // 1 (row number) + 1 (extra source line)
        }
        line_index += 1;
    }

    let width = format!("{}", largest_line_number).len();

    line_index = 1;
    while line_index < line_spans.len() {
        if span.start >= line_spans[line_index].0 && span.start <= line_spans[line_index].1 {
            let column_index = span.start - line_spans[line_index].0;

            eprintln!(
                "----- \u{001b}[33m{}:{}:{}\u{001b}[0m",
                filename,
                line_index + 1,
                column_index + 1
            );

            if line_index > 0 {
                print_source_line(
                    severity,
                    file_contents,
                    line_spans[line_index - 1],
                    span,
                    line_index,
                    largest_line_number,
                );
            }

            print_source_line(
                severity,
                file_contents,
                line_spans[line_index],
                span,
                line_index + 1,
                largest_line_number,
            );

            for _ in 0..(span.start - line_spans[line_index].0 + width + 4) {
                eprint!(" ")
            }

            eprintln!(
                "\u{001b}[{}m^- {}\u{001b}[0m",
                severity.ansi_color_code(),
                message
            );

            while (line_index < line_spans.len()) && (span.end > line_spans[line_index].0) {
                line_index += 1;
                if line_index >= line_spans.len() {
                    break;
                }

                print_source_line(
                    severity,
                    file_contents,
                    line_spans[line_index],
                    span,
                    line_index + 1,
                    largest_line_number,
                );
            }
        } else {
            line_index += 1;
        }
    }
    eprintln!("\u{001b}[0m-----");
}

fn print_source_line(
    severity: MessageSeverity,
    file_contents: &[u8],
    file_span: (usize, usize),
    error_span: Span,
    line_number: usize,
    largest_line_number: usize,
) {
    let mut index = file_span.0;

    let largest_width = format!("{}", largest_line_number).len();
    let current_width = format!("{}", line_number).len();

    eprint!(" {}", line_number);
    for _ in 0..(largest_width - current_width) {
        eprint!(" ");
    }
    eprint!(" | ");

    while index <= file_span.1 {
        let mut c = b' ';
        if index < file_span.1 {
            c = file_contents[index]
        } else if error_span.start == error_span.end && index == error_span.start {
            c = b'_'
        }

        if index == error_span.start {
            eprint!("\u{001b}[{}m", severity.ansi_color_code())
        }
        if index == error_span.end {
            eprint!("\u{001b}[0m")
        }

        eprint!("{}", c as char);

        index += 1
    }
    eprintln!();
}

fn gather_line_spans(file_contents: &[u8]) -> Vec<(usize, usize)> {
    let mut idx = 0;
    let mut output = vec![];

    let mut start = idx;
    while idx < file_contents.len() {
        if file_contents[idx] == b'\n' {
            output.push((start, idx));
            start = idx + 1;
        }
        idx += 1;
    }
    if start < idx {
        output.push((start, idx));
    }

    output
}
