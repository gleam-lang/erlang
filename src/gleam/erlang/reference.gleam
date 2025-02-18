/// A unique reference value.
///
/// It holds no particular meaning or value, but unique values are often useful
/// in programs are used heavily within both Gleam and Erlang's OTP frameworks.
///
/// More can be read about references in the [Erlang documentation][1].
///
/// [1]: https://www.erlang.org/doc/efficiency_guide/advanced.html#unique_references
///
pub type Reference

/// Create a new unique reference.
///
@external(erlang, "erlang", "make_ref")
pub fn new() -> Reference
