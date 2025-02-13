//// A charlist is a list of integers where all the integers are valid code
//// points.
////
//// In practice, you will not come across them often, except perhaps when
//// interfacing with Erlang, in particular when using older libraries that do
//// not accept binaries as arguments.

/// A list of characters represented as ints. Commonly used with Erlang
/// functions that do not accept binary strings such as Gleam's core string
/// type.
///
pub type Charlist

/// Convert a charlist to a string using Erlang's
/// `unicode:characters_to_binary`.
///
@external(erlang, "unicode", "characters_to_binary")
pub fn to_string(a: Charlist) -> String

/// Convert a string to a charlist using Erlang's
/// `unicode:characters_to_list`.
///
@external(erlang, "unicode", "characters_to_list")
pub fn from_string(a: String) -> Charlist
