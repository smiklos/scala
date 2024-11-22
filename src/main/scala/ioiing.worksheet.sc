import cats.* 
import cats.effect.IO

IO.pure(42)

val x  = 15

IO(x * 2).map(x => x * 42)