def sortInt(list: List[Int]): List[Int] = list match
    case Nil => Nil
    case head :: next => {
        val (smaller, higher) = next.partition(e => e < head)
        smaller ++ List(head) ++ higher
}

val nums = List(2, 4, 1, 1, 5)

val sorted = sortInt(nums)

println(sorted)

//how to generalize it to work on complex type too

// OOP

trait Comparable[T] {

  def compare(other: T): Int
}

object Comparable {

  extension [T](c: Comparable[T]) {
    def <(other: T): Boolean = c.compare(other) < 0
  }
}

def sort[T <: Comparable[T]](list: List[T]): List[T] = list match
  case Nil => Nil
  case head :: next => {
    val (smaller, higher) = next.partition(e => e < head)
    sort(smaller) ++ List(head) ++ sort(higher)
  }

extension [T <: Comparable[T]](list: List[T]) def ordered: List[T] = sort(list)

//how to sort people

case class Person(name: String, age: Int) extends Comparable[Person] {

  def compare(other: Person): Int = age - other.age

}

val peeps = List(Person("Miklos", 35), Person("Cagla", 25), Person("Sadek", 20))

println(sort(peeps))


//But now , can no longer sort ints 

//sort(List(4,2,6,5))

// Need a solution that we can use for types we don't own

//first explore an approach without given instances using currying


trait Ordering[T]:
  def compare(x: T, y: T): Int

  def on[Y](f: Y => T): Ordering[Y] = (x: Y, y: Y) => compare(f(x), f(y))


object Ordering {
  extension [A](l: A)(using order: Ordering[A])
    def <(r: A): Boolean = order.compare(l, r) < 0

  given Ordering[Int] with
    def compare(x: Int, y: Int): Int = x - y

  given Ordering[String] with 
    def compare(x: String, y: String): Int = x.compareTo(y)
}

import Ordering._  
// Implement the sort function using the Ordering type class
def sortAnything[T](list: List[T])(using ord: Ordering[T]): List[T] = list match
  case Nil => Nil
  case head :: next =>
    val (smaller, higher) = next.partition(e => e < head)
    sortAnything(smaller) ++ List(head) ++ sortAnything(higher)

// Provide an extension method to sort lists of any type with an Ordering instance
extension [T](list: List[T])(using ord: Ordering[T])
  def ordered: List[T] = sortAnything(list)

// extension [T: Ordering](list: List[T])
//   def ordered: List[T] = sortAnything(list)  

// Provide an Ordering instance for Person
given Ordering[Person] with
  def compare(x: Person, y: Person): Int = x.age - y.age

sortAnything(peeps)
sortAnything(List(4,2,6,5))

// Reuse existing ordering instances to derive new ones

//val personOrdering: Ordering[Person] = summon[Ordering[Int]].on(_.age)

// I don't want to manually write case class Ordering instaces, I want to derive them

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}


// We can move this to Ordering and support derivation directly on any case class 

object OrderingDerivation {

    inline def summonAsList[T <: Tuple]: List[Ordering[?]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Ordering[t]] :: summonAsList[ts]
    }


    inline def derived[T](using m: Mirror.Of[T]): Ordering[T] = {
    val elemInstances = summonAsList[m.MirroredElemTypes]
    new Ordering[T] {
        def compare(x: T, y: T): Int = {
        val xProduct = x.asInstanceOf[Product].productIterator
        val yProduct = y.asInstanceOf[Product].productIterator

        var acc = 0
        val it = xProduct.zip(yProduct).zip(elemInstances).iterator
        while (it.hasNext && acc == 0) {
            val ((xElem, yElem), ord) = it.next()
            acc = ord.asInstanceOf[Ordering[Any]].compare(xElem, yElem)
        }
        acc
        }
    }
    }
}

import OrderingDerivation._

case class PersonPro(name: String, age: Int)


// we can also add this back to the normal person class

object PersonPro {
  given Ordering[PersonPro] = OrderingDerivation.derived
}

sortAnything(List(PersonPro("Miklos", 35), PersonPro("Cagla", 25), PersonPro("Sadek", 20)))



// Can we solve this with opaque types and implicit conversions

// write an opaque type that wraps an Int and provides a compare method

// NOT WORKING AT ALL 


// opaque type ComparableInt = Int

// object ComparableInt:
//   // Provide an extension method to convert Int to ComparableInt
//   extension (x: Int)
//     def toComparableInt: ComparableInt = x

//   // Provide an extension method to implement the compare method for ComparableInt
//   extension (x: ComparableInt)
//     def compare(other: ComparableInt): Int = x - other

//  given Conversion[Int, Comparable[Int]] = i => new Comparable[Int] {
//         override def compare(other: Int): Int = i - other
//     }

// import ComparableInt._

// val l: List[ComparableInt] = List(4,2,6,5).map(_.toComparableInt)
// sort(l)
//monads and direct style effects

//write a complete effect system that can handle IO

