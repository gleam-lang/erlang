import gleam/erlang/env
import gleam/map

pub fn get_all_test() {
  env.set("MYVAR", "MYVALUE")

  let vars = env.get_all()
  assert Ok("MYVALUE") = map.get(vars, "MYVAR")

  env.unset("MYVAR")
}

pub fn set_get_test() {
  assert Nil = env.set("MYVAR", "MYVALUE")
  assert Ok("MYVALUE") = env.get("MYVAR")

  assert Nil = env.set(name: "MYVAR", value: "MYVALUE")
  assert Ok("MYVALUE") = env.get(name: "MYVAR")

  env.unset("MYVAR")
}

pub fn unset_test() {
  env.set("MYVAR", "MYVALUE")

  assert Nil = env.unset("MYVAR")
  assert Error(Nil) = env.get("MYVAR")

  env.set("MYVAR", "MYVALUE")

  assert Nil = env.unset(name: "MYVAR")
  assert Error(Nil) = env.get(name: "MYVAR")
}

pub fn get_non_existant_test() {
  // Just to make sure
  env.unset("I_DONT_EXIST")

  assert Error(Nil) = env.get("I_DONT_EXIST")
  assert Nil = env.unset("I_DONT_EXIST")
}
