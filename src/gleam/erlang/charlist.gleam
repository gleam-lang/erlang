//// A charlist is a list of integers where all the integers are valid code
//// points.
////
//// In practice, you will not come across them often, except perhaps when
//// interfacing with Erlang, in particular when using older libraries that do
//// not accept binaries as arguments.

/// A list of characters represented as ints. Commonly used by older Erlang
/// modules.
pub external type Charlist

/// Transform a charlist to a string
/// Note: Erlang default encoding is utf8, so we're good
pub external fn to_string(Charlist) -> String =
  "unicode" "characters_to_binary"

/// Transform a string to a charlist
/// Note: Erlang default encoding is utf8, so we're good
pub external fn from_string(String) -> Charlist =
  "unicode" "characters_to_list"
