// Everything is a value
import scala.util.Try

//representing empty values
val maybeString: Option[String] = Some("Hello")
val emptyString: Option[String] = None


//representing error values
val maybeAGoodresult =  Try(42 / 2)
val maybeAResult = Try(42 / 0)

//representing custom error values
enum MyError {
  case NotFound
  case Invalid
}

val maybeGoodResult: Either[MyError, Int] = Right(42)
val maybeBadResult: Either[MyError, Int] = Left(MyError.Invalid)

// all these values have contextual parameter and we typically need to work with them in a functional way. (again, no mutation)
maybeAGoodresult.map(old => old * 2)
maybeGoodResult.map(old => old * 2)



//how to create reusable code for these values? Using higher order functions of course. These functions can be passed around and can be used to map the values to new values.
//why higher order functions? because functions are first class citizens in scala

def mapOption[A, B](maybeA: Option[A], f: A => B): Option[B] = maybeA.map(f)
def mapTry[A, B](maybeA: Try[A], f: A => B): Try[B] = maybeA.map(f)

def multipleValueIfPresent(maybe: Option[Int]): Option[Int] = mapOption(maybe, x => x * 2)

//FP loves revcursive data structures. They have good support for immutable operations and pattern matching

//And if we want to work with multiple values, we can use lists

val list = List(1, 2, 3, 4, 5)

def mapValues[A, B](list: List[A], f: A => B): List[B] = list.map(f)

list.map(_ * 2) == mapValues(list, _ * 2)

// How to introduce code that can be reused on these values? What do they share in common? They are all containers of values.

// Naive approach is to create a trait and implement the trait for each of the types. But this is not scalable.

trait Mapper[A] { self => 
  def map[B](f: A => B): Mapper[B]
}

trait Test[T] extends Mapper[T] {
  def map[B](f: T => B): Test[B] = this match {
    case No => No.asInstanceOf[Test[B]]
    case Yes(data) => Yes(f(data))
  }
}

case object No extends Test[Nothing]
case class Yes[T](data: T) extends Test[T]

//Why linkedlists? they are easy to retain as immutable and destructure
// because without mutating the objects we map them to new state via functions and a lot of our structures are immutable and contextual. 


// introduce higher kinded types

// We can create abstraction over higher kinded types with typeclasses and depend on the typeclass instances to provide the implementation


//Why is this useful? We can create reusable code in libraries and let the developers plug in types on the call site. Some projects are working this way (tagless final) to track the side effects/capabilities and abstract over the effectful computations. 

