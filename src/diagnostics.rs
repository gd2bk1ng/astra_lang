use crate::parser::ParseError;
use crate::tokens::Span;
use std::fmt;

/// Diagnostic severity emitted by the SyntraLang parser infrastructure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Note,
}

/// A source-aware diagnostic whose location remains tied to the original input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Diagnostic<'a> {
    pub severity: DiagnosticSeverity,
    pub code: &'static str,
    pub message: String,
    pub span: Span,
    pub source: &'a str,
}

impl<'a> Diagnostic<'a> {
    pub fn source_line(&self) -> &'a str {
        let start = line_start(self.source, self.span.start);
        let end = line_end(self.source, self.span.start);
        &self.source[start..end]
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.span.start..self.span.end
    }

    pub fn render(&self) -> String {
        let line = self.source_line();
        let width = self
            .source
            .get(self.span.start..self.span.end)
            .map(|slice| slice.chars().count().max(1))
            .unwrap_or(1);

        let caret_column = self
            .source
            .get(line_start(self.source, self.span.start)..self.span.start)
            .map(|prefix| prefix.chars().count())
            .unwrap_or(0);

        format!(
            "{}[{}]: {}\n --> {}:{} (bytes {}..{})\n  | {}\n  | {}{}",
            self.severity,
            self.code,
            self.message,
            self.span.line,
            self.span.column,
            self.span.start,
            self.span.end,
            line,
            " ".repeat(caret_column),
            "^".repeat(width),
        )
    }
}

impl fmt::Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
            Self::Note => write!(f, "note"),
        }
    }
}

/// Converts parser errors into stable, source-aware diagnostics.
#[derive(Debug, Default, Clone, Copy)]
pub struct DiagnosticEngine;

impl DiagnosticEngine {
    pub fn parse_error<'a>(source: &'a str, error: ParseError<'a>) -> Diagnostic<'a> {
        let (code, message) = match &error {
            ParseError::UnexpectedEof { expected, .. } => (
                "SYN1001",
                format!("unexpected end of input; expected {expected}"),
            ),
            ParseError::UnexpectedToken {
                expected, found, ..
            } => ("SYN1002", format!("expected {expected}, found {found:?}")),
        };

        let span = match error {
            ParseError::UnexpectedEof { span, .. }
            | ParseError::UnexpectedToken { span, .. } => span,
        };

        Diagnostic {
            severity: DiagnosticSeverity::Error,
            code,
            message,
            span,
            source,
        }
    }

    pub fn parse_errors<'a>(
        source: &'a str,
        errors: impl IntoIterator<Item = ParseError<'a>>,
    ) -> Vec<Diagnostic<'a>> {
        errors
            .into_iter()
            .map(|error| Self::parse_error(source, error))
            .collect()
    }
}

fn line_start(source: &str, offset: usize) -> usize {
    let offset = offset.min(source.len());
    source[..offset].rfind('\n').map_or(0, |index| index + 1)
}

fn line_end(source: &str, offset: usize) -> usize {
    let start = line_start(source, offset);
    source[start..]
        .find('\n')
        .map_or(source.len(), |index| start + index)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Lexer, Parser};

    #[test]
    fn renders_precise_source_location() {
        let source = "fn main() {\n  return ;\n}";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let error = parser.parse_program().unwrap_err();
        let diagnostic = DiagnosticEngine::parse_error(source, error);

        assert_eq!(diagnostic.span.line, 2);
        assert_eq!(diagnostic.span.column, 10);
        assert_eq!(diagnostic.byte_range(), 21..21);
        assert!(diagnostic.render().contains(" --> 2:10 (bytes 21..21)"));
        assert!(diagnostic.render().contains("return ;"));
    }
}
