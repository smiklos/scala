import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline, constValue}

trait Encoder[T] {
  def encode(value: T): String
}

object Encoder {
  given Encoder[Int] with {
    def encode(value: Int): String = value.toString
  }

  given Encoder[String] with {
    def encode(value: String): String = s""""$value""""
  }

  given Encoder[Boolean] with {
    def encode(value: Boolean): String = if value then "true" else "false"
  }

    given [T](using enc: Encoder[T]): Encoder[List[T]] with {
    def encode(value: List[T]): String = value.map(enc.encode).mkString("[", ", ", "]")
  }

  inline def summonAsList[T <: Tuple]: List[Encoder[?]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Encoder[t]] :: summonAsList[ts]
  }

  inline def summonFieldNames[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => constValue[t].toString :: summonFieldNames[ts]
  }

  inline def derived[T](using m: Mirror.Of[T]): Encoder[T] = {
    val elemInstances = summonAsList[m.MirroredElemTypes]
    val fieldNames = summonFieldNames[m.MirroredElemLabels]
    new Encoder[T] {
      def encode(value: T): String = {
        val valueProduct = value.asInstanceOf[Product].productIterator
        val inner = valueProduct.zip(elemInstances.zip(fieldNames)).foldLeft("") {
          case (acc, (elem, (encoder, fieldName))) =>
            acc + s"\"$fieldName\": " + encoder.asInstanceOf[Encoder[Any]].encode(elem) + ", "
        }.dropRight(2) // Remove the trailing comma and space

        s"{ $inner }"
      }
    }
  }
}

case class PersonPro(name: String, age: Int, foods: List[String]) derives Encoder

Encoder.derived[PersonPro].encode(PersonPro("John", 30, List("apple", "banana")))