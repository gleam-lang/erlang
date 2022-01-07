import gleam/erlang/os
import gleam/map

pub fn get_all_test() {
  os.set("MYVAR", "MYVALUE")

  let vars = os.get_all()
  assert Ok("MYVALUE") = map.get(vars, "MYVAR")

  os.unset("MYVAR")
}

pub fn set_get_test() {
  assert Nil = os.set("MYVAR", "MYVALUE")
  assert Ok("MYVALUE") = os.get("MYVAR")

  assert Nil = os.set(name: "MYVAR", value: "MYVALUE")
  assert Ok("MYVALUE") = os.get(name: "MYVAR")

  os.unset("MYVAR")
}

pub fn unset_test() {
  os.set("MYVAR", "MYVALUE")

  assert Nil = os.unset("MYVAR")
  assert Error(Nil) = os.get("MYVAR")

  os.set("MYVAR", "MYVALUE")

  assert Nil = os.unset(name: "MYVAR")
  assert Error(Nil) = os.get(name: "MYVAR")
}

pub fn get_non_existant_test() {
  // Just to make sure
  os.unset("I_DONT_EXIST")

  assert Error(Nil) = os.get("I_DONT_EXIST")
  assert Nil = os.unset("I_DONT_EXIST")
}
