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
pub external fn to_string(Charlist) -> String =
  "erlang" "list_to_binary"

/// Transform a string to a charlist
pub external fn from_string(String) -> Charlist =
  "erlang" "binary_to_list"
