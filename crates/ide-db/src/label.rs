//! See [`Label`]
use std::fmt;

use stdx::always;

/// A type to specify UI label, like an entry in the list of assists. Enforces
/// proper casing:
///
///    Frobnicate bar
///
/// Note the upper-case first letter and the absence of `.` at the end.
#[derive(Clone)]
pub struct Label(String);

impl PartialEq<str> for Label {
    fn eq(
        &self,
        other: &str,
    ) -> bool {
        self.0 == other
    }
}

impl PartialEq<&'_ str> for Label {
    fn eq(
        &self,
        other: &&str,
    ) -> bool {
        self == *other
    }
}

impl From<Label> for String {
    fn from(label: Label) -> Self {
        label.0
    }
}

impl Label {
    pub fn new(label: String) -> Self {
        always!(label.starts_with(char::is_uppercase) && !label.ends_with('.'));
        Self(label)
    }
}

impl fmt::Display for Label {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}

impl fmt::Debug for Label {
    fn fmt(
        &self,
        #[expect(clippy::min_ident_chars, reason = "trait impl")] f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}
