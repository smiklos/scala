def sortInt(nums: List[Int]): List[Int]= nums match
    case head :: rest =>
        val (left, right) = rest.partition(elem => (elem - head) < 0)
        sortInt(left) ++ List(head) ++ sortInt(right)
    case Nil => Nil
 

def sortComparable[T <: Comparable[T]](elems: List[T]): List[T]= elems match
    case head :: rest =>
        val (left, right) = rest.partition(elem => (elem.compareTo(head)) < 0)
        sortComparable(left) ++ List(head) ++ sortComparable(right)
    case Nil => Nil
 



sortInt(List(2,1,4,7))




trait Comparable[T] {

    def compareTo(another: T): Int
}


case class Person(age: Int, name: String) extends Comparable[Person] {

    override def compareTo(another: Person): Int = age - another.age
}

object Person {

      given Sorter[Person] with
        def compare(a: Person, b: Person): Int =  a.age - b.age
}

sortComparable(List(Person(35, "Miklos"),Person(20, "Sadek"), Person(25, "Calga")))

//sortComparable(List(2,1,4,6))

trait Sorter[T] {

    def compare(a: T, b: T): Int
}

object Sorter {

      extension [A](l: A)(using order: Sorter[A])
        def <(r: A): Boolean = order.compare(l, r) < 0
 
}

import Sorter._

def sortTypeclass[T : Sorter](elems: List[T]): List[T]= elems match
    case head :: rest =>
        val (left, right) = rest.partition(elem => elem < head)
        sortTypeclass(left) ++ List(head) ++ sortTypeclass(right)
    case Nil => Nil
 
given intSorter: Sorter[Int] = new Sorter[Int] {

     def compare(a: Int, b: Int): Int = a - b
}

sortTypeclass(List(2,1,8,4))
sortTypeclass(List(Person(35, "Miklos"),Person(20, "Sadek"), Person(25, "Calga")))

