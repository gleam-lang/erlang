import gleam/dynamic
import gleam/dynamic/decode

/// Atom is a special string-like data-type that is most commonly used for
/// interfacing with code written in other BEAM languages such as Erlang and
/// Elixir. It is preferable to define your own custom types to use instead of
/// atoms where possible.
///
/// Atoms are not used much in typical Gleam code!
///
/// ## Creating atoms
///
/// We can create atoms with the the [`create`](#create)
/// function, though we must be careful when doing so as atoms are never
/// garbage collected. If we create too many atoms (for example, if we convert
/// user input into atoms) we may hit the max limit of atoms and cause the
/// virtual machine to crash.
///
pub type Atom

/// Finds an existing atom for the given string.
///
/// If no atom is found in the virtual machine's atom table for the string then
/// an error is returned.
///
@external(erlang, "gleam_erlang_ffi", "atom_from_string")
pub fn get(a: String) -> Result(Atom, Nil)

/// Creates an atom from a string, inserting a new value into the virtual
/// machine's atom table if an atom does not already exist for the given
/// string.
///
/// We must be careful when using this function as there is a limit to the
/// number of atom that can fit in the virtual machine's atom table. Never
/// convert user input into atoms as filling the atom table will cause the
/// virtual machine to crash!
///
@external(erlang, "erlang", "binary_to_atom")
pub fn create(a: String) -> Atom

/// Returns a `String` corresponding to the text representation of the given
/// `Atom`.
///
/// ## Examples
/// ```gleam
/// let ok_atom = create("ok")
/// to_string(ok_atom)
/// // -> "ok"
/// ```
///
@external(erlang, "erlang", "atom_to_binary")
pub fn to_string(a: Atom) -> String

/// Convert an atom to a dynamic value, throwing away the type information. 
///
/// This may be useful for testing decoders.
///
@external(erlang, "gleam_erlang_ffi", "identity")
pub fn to_dynamic(a: Atom) -> dynamic.Dynamic

@external(erlang, "gleam_erlang_ffi", "identity")
pub fn cast_from_dynamic(a: dynamic.Dynamic) -> Atom

/// A dynamic decoder for atoms.
///
/// You almost certainly should not use this to work with externally defined
/// functions. They return known types, so you should define the external
/// functions with the correct types, defining wrapper functions in Erlang if
/// the external types cannot be mapped directly onto Gleam types.
///
pub fn decoder() -> decode.Decoder(Atom) {
  decode.new_primitive_decoder("Atom", fn(data) {
    case is_atom(data) {
      True -> Ok(cast_from_dynamic(data))
      False -> Error(create("nil"))
    }
  })
}

@external(erlang, "erlang", "is_atom")
fn is_atom(data: dynamic.Dynamic) -> Bool
