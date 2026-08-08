use std::{error, range::Range};

use base_db::{EditionedFileId, FileRange};
use hir::{HirDatabase, diagnostics::AnyDiagnostic};
use rowan::{TextRange, TextSize};

use crate::DiagnosticsConfig;

type Module = naga::Module;
type ParseError = naga::front::wgsl::ParseError;
type ValidationError = naga::WithSpan<naga::valid::ValidationError>;

fn parse(source: &str) -> Result<Module, ParseError> {
    naga::front::wgsl::parse_str(source)
}

fn validate(module: &Module) -> Result<(), ValidationError> {
    let flags = naga::valid::ValidationFlags::all();
    let capabilities = naga::valid::Capabilities::all();
    let mut validator = naga::valid::Validator::new(flags, capabilities);
    validator.validate(module).map(drop)
}

impl NagaError for naga::front::wgsl::ParseError {
    fn spans(&self) -> Box<dyn Iterator<Item = (Option<Range<usize>>, String)> + '_> {
        Box::new(
            self.labels()
                .map(|(span, label)| (to_range(span), label.to_owned())),
        )
    }

    fn location(&self) -> Option<Range<usize>> {
        let (span, _) = self.labels().next()?;
        to_range(span)
    }
}

impl NagaError for naga::WithSpan<naga::valid::ValidationError> {
    fn spans(&self) -> Box<dyn Iterator<Item = (Option<Range<usize>>, String)> + '_> {
        Box::new(
            self.spans()
                .map(move |(span, label)| (to_range(*span), label.clone())),
        )
    }

    fn location(&self) -> Option<Range<usize>> {
        self.spans().next().and_then(|(span, _)| to_range(*span))
    }
}

fn to_range(span: naga::Span) -> Option<Range<usize>> {
    span.to_range().map(Range::from)
}

pub(crate) trait NagaError: error::Error {
    fn spans(&self) -> Box<dyn Iterator<Item = (Option<Range<usize>>, String)> + '_>;
    fn location(&self) -> Option<Range<usize>>;
}

fn emit<Error>(
    database: &dyn HirDatabase,
    error: &Error,
    file_id: EditionedFileId,
    full_range: TextRange,
    accumulator: &mut Vec<AnyDiagnostic>,
) where
    Error: NagaError,
{
    let message = error_message_cause_chain(&error);
    let original_range = |range: Range<usize>| {
        TextRange::new(
            TextSize::from(u32::try_from(range.start).expect("indexes are small numbers")),
            TextSize::from(u32::try_from(range.end).expect("indexes are small numbers")),
        )
    };
    let location = error.location().map_or(full_range, original_range);

    let spans = error.spans().filter_map(|(span, label)| {
        let range = original_range(span?);
        Some((range, label))
    });

    let related: Vec<_> = spans
        .map(|(range, message)| {
            (
                message,
                FileRange {
                    range,
                    file_id: file_id.file_id(database),
                },
            )
        })
        .collect();

    accumulator.push(AnyDiagnostic::NagaValidationError {
        file_id,
        range: location,
        message,
        related,
    });
}

pub(crate) fn naga_diagnostics(
    database: &dyn HirDatabase,
    file_id: EditionedFileId,
    config: &DiagnosticsConfig,
    accumulator: &mut Vec<AnyDiagnostic>,
) {
    let source: &str = database.file_text(file_id.file_id(database)).text(database);
    let full_range = TextRange::up_to(TextSize::of(source));

    match parse(source) {
        Ok(module) => {
            if !config.naga_validation_enabled {
                return;
            }
            if let Err(error) = validate(&module) {
                emit(database, &error, file_id, full_range, accumulator);
            }
        },
        Err(error) => {
            if !config.naga_parsing_enabled {
                return;
            }
            emit(database, &error, file_id, full_range, accumulator);
        },
    }
}

fn error_message_cause_chain(error: &dyn error::Error) -> String {
    let mut message = error.to_string();

    let mut error = error.source();
    if error.is_some() {
        message.push_str(": ");
    }

    while let Some(source) = error {
        message.push_str(&source.to_string());
        error = source.source();
    }

    message
}
