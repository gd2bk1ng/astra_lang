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
        let offset = safe_offset(self.source, self.span.start);
        let start = line_start(self.source, offset);
        let end = line_end(self.source, offset);
        &self.source[start..end]
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.span.start..self.span.end
    }

    pub fn render(&self) -> String {
        let safe_start = safe_offset(self.source, self.span.start);
        let line = self.source_line();
        let valid_range = self.span.start <= self.span.end
            && self.span.end <= self.source.len()
            && self.source.is_char_boundary(self.span.start)
            && self.source.is_char_boundary(self.span.end);
        let width = if valid_range {
            self.source
                .get(self.span.start..self.span.end)
                .map(|slice| slice.chars().count().max(1))
                .unwrap_or(1)
        } else {
            1
        };

        let caret_column = self
            .source
            .get(line_start(self.source, safe_start)..safe_start)
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

    /// Construct a source-aware diagnostic for a semantic verification error.
    pub fn semantic_error<'a>(
        source: &'a str,
        code: &'static str,
        message: impl Into<String>,
        span: Span,
    ) -> Diagnostic<'a> {
        Diagnostic {
            severity: DiagnosticSeverity::Error,
            code,
            message: message.into(),
            span,
            source,
        }
    }
}

/// Clamp an arbitrary byte offset into the source and retreat to a UTF-8
/// character boundary. This normalization is rendering-only: the diagnostic's
/// original span and reported byte coordinates are never modified.
fn safe_offset(source: &str, offset: usize) -> usize {
    let mut offset = offset.min(source.len());
    while !source.is_char_boundary(offset) {
        offset -= 1;
    }
    offset
}

fn line_start(source: &str, offset: usize) -> usize {
    let offset = safe_offset(source, offset);
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

    fn malformed_diagnostic<'a>(source: &'a str, start: usize, end: usize) -> Diagnostic<'a> {
        DiagnosticEngine::semantic_error(
            source,
            "SYNTEST",
            "malformed span",
            Span::with_pos(start, end, 7, 13),
        )
    }

    #[test]
    fn rendering_is_panic_free_for_malformed_span_boundaries() {
        let source = "aé🙂\nnext";
        let malformed_spans = [
            (source.len() + 8, source.len() + 12),
            (2, 4), // start is inside the two-byte encoding of é
            (0, 2), // end is inside the two-byte encoding of é
            (0, source.len() + 1),
            (5, 2), // reversed range
            (source.len(), 0), // reversed range at end of source
        ];

        for (start, end) in malformed_spans {
            let diagnostic = malformed_diagnostic(source, start, end);
            assert!(
                std::panic::catch_unwind(|| diagnostic.source_line()).is_ok(),
                "source_line panicked for span {start}..{end}"
            );
            assert!(
                std::panic::catch_unwind(|| diagnostic.render()).is_ok(),
                "render panicked for span {start}..{end}"
            );
            assert_eq!(diagnostic.span.start, start);
            assert_eq!(diagnostic.span.end, end);
            assert_eq!(diagnostic.byte_range(), start..end);
        }
    }

    #[test]
    fn invalid_utf8_start_retreats_to_preceding_character_boundary_for_rendering() {
        let source = "aé🙂\nnext";
        let diagnostic = malformed_diagnostic(source, 2, 4);

        assert_eq!(diagnostic.source_line(), "aé🙂");
        let rendered = diagnostic.render();
        assert!(rendered.contains(" --> 7:13 (bytes 2..4)"));
        assert!(rendered.contains("  | ^"));
    }

    #[test]
    fn invalid_ranges_use_one_caret_without_rewriting_reported_offsets() {
        let source = "first line\nsecond";
        let diagnostic = malformed_diagnostic(source, 13, 999);
        let rendered = diagnostic.render();

        assert!(rendered.contains(" --> 7:13 (bytes 13..999)"));
        assert!(rendered.ends_with("  |   ^"));
        assert_eq!(diagnostic.span.start, 13);
        assert_eq!(diagnostic.span.end, 999);
    }

    #[test]
    fn empty_source_and_extreme_offsets_render_safely() {
        let diagnostic = malformed_diagnostic("", usize::MAX, 0);

        assert_eq!(diagnostic.source_line(), "");
        assert!(diagnostic.render().contains("(bytes "));
        assert!(diagnostic.render().ends_with("  | ^"));
    }

    #[test]
    fn valid_unicode_span_keeps_original_rendering_and_highlight_width() {
        let source = "prefix\né🙂x";
        let start = "prefix\n".len();
        let end = start + "é🙂".len();
        let diagnostic = DiagnosticEngine::semantic_error(
            source,
            "SYNTEST",
            "valid unicode span",
            Span::with_pos(start, end, 2, 1),
        );

        assert_eq!(diagnostic.source_line(), "é🙂x");
        assert_eq!(
            diagnostic.render(),
            "error[SYNTEST]: valid unicode span\n --> 2:1 (bytes 7..13)\n  | é🙂x\n  | ^^"
        );
    }

    #[test]
    fn renders_precise_source_location() {
        let source = "fn main() {\n  return ;\n}";
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let error = parser.parse_program().unwrap_err();
        let diagnostic = DiagnosticEngine::parse_error(source, error);

        assert_eq!(diagnostic.span.line, 2);
        assert_eq!(diagnostic.span.column, 10);
        assert_eq!(diagnostic.byte_range(), 21..22);
        assert!(diagnostic.render().contains(" --> 2:10 (bytes 21..22)"));
        assert!(diagnostic.render().contains("return ;"));
    }
}
