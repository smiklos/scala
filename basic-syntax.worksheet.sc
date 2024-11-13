val variable = 5

val l = List("Cagla", "Sadek")

l ++ List("Miklos")

val y = 10

def doesSomething(x: Int)(y: Int): Int | String = {

  println(s"$x is cool")
  if (x % 2 == 0) {
    x * y
  } else {
    "Can't compute"
  }
}

def workWithF(f: Int => Int) = f(41)

val somethingWithY = doesSomething(11)(_)

val res = somethingWithY(4)

workWithF(x => x * 2)

class MyClass {

  def doSomething(x: Int): Int = {
    x * 2
  }
}
