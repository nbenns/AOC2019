type A = {
  val console: Unit
}

type B = {
  val logging: Unit
}

type C = A with B

val a = new {
  val console = ()
}

val b = new {
  val logging = ()
}

def mix(a: A, b: B): C = new {
  val console = a.console
  val logging = b.logging
}

mix(a, b).logging






