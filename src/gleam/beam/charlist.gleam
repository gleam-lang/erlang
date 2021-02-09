//// Casting erlang charlist to and from a gleam string.
////
//// **A charlist is a list of integers where all the integers are valid code points.**
////  In practice, you will not come across them often, except perhaps when interfacing with Erlang,
//// in particular when using older libraries that do not accept binaries as arguments.

// https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#charlists
// Elixir makes charlist one word

/// List of codepoints
pub external type Charlist

/// Transform a charlist to a string
pub external fn to_string(Charlist) -> String =
  "erlang" "list_to_binary"

/// Transform a string to a charlist
pub external fn from_string(String) -> Charlist =
  "erlang" "binary_to_list"
