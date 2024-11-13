import scala.annotation.tailrec
import scala.util.*


//introduce ref transparency and composability
enum IO[+T] {
  case Pure(t: T)
  case Effect(t: () => T)

  case FlatMap[T, Y](io: IO[T], mapper: T => IO[Y]) extends IO[Y]

  def map[Y](mapper: T => Y): IO[Y] =
    FlatMap[T, Y](this, t => Effect(() => mapper(t)))

  def flatMap[Y](mapper: T => IO[Y]) = FlatMap[T, Y](this, mapper)
}

object IO {

  def pure[T](t: T) = Pure(t)

  def effect[T](t: => T) = Effect(() => t)
}

def printLn(statement: String) = IO.pure(println(statement))

//@tailrec
//make eval tail recursive

def evalDummy[T](io: IO[T]): T = io match
  case IO.Pure(t)             => t
  case IO.FlatMap(io, mapper) => evalDummy(mapper(evalDummy(io)))
  case IO.Effect(t)             => t()

final def eval[T](io: IO[T]): Try[T] = io match
  case IO.Pure(t)             => Try(t)
  case IO.Effect(t)             => Try(t())
  case IO.FlatMap(io, mapper) => eval(io).flatMap(e => eval(mapper(e)))


// final def evalRec[T](io: IO[T]): Try[T] = {
//   @tailrec
//   def loop(stack: List[IO[Any]], result: Try[Any]): Try[Any] = stack match {
//     case Nil => result
//     case IO.Pure(t) :: tail => loop(tail, result.flatMap(_ => Try(t())))
//     case IO.FlatMap(mapper, io) :: tail => loop(mapper.asInstanceOf[Any => IO[Any]](result.get) :: io :: tail, result)
//   }

//   loop(List(io), Try(io)).asInstanceOf[Try[T]]
// }

//can try this if I call it pure and Effect/Apply. 

@tailrec
final def evalRec[B](in: IO[B]): B = in match {
  case IO.Pure(a) => a
  case IO.Effect(r) => evalRec(IO.Pure(r()))
  case IO.FlatMap(x, f) => x match {
    case IO.Pure(a) => evalRec(f(a))
    case IO.Effect(r) => evalRec(IO.FlatMap(IO.Pure(r()), f))
    case IO.FlatMap(y, g) => evalRec(y.flatMap(g(_) flatMap f))
  }
}

@tailrec
final def evalRecSafe[B](in: IO[B]): Try[B] = in match {
  case IO.Pure(a) => Try(a)
  case IO.Effect(r) => Try(r()) match
    case Failure(exception) => Failure(exception)
    case Success(value) => evalRecSafe(IO.Pure(value))
  case IO.FlatMap(x, f) => x match {
    case IO.Pure(a) => evalRecSafe(f(a))
    case IO.Effect(r) => evalRecSafe(IO.FlatMap(IO.Pure(r()), f))
    case IO.FlatMap(y, g) => evalRecSafe(y.flatMap(g(_) flatMap f))
  }
}



val complexStuff = IO
  .pure(42)
  .map(x => x * 2)
  .flatMap(x =>
    printLn(s"I'm printing $x")
      .map(_ => x)
  )

//a combinator for any IO value
def repeat[T](io: IO[T], times: Int): IO[T] =
  if times > 1 then io.flatMap(_ => repeat(io, times - 1)) else io


def fact(num: Long): IO[Long] =  { 
  // recursive factorial using effects
  if num == 0 then IO.pure(1L) else IO.effect(fact(num - 1).map(_ * num)).flatMap(identity)
}  
val repated10x = repeat(complexStuff, 10)

eval(repated10x)


val repatedManyx = repeat(complexStuff, 10000)

//fix the stack overflow issue

//Add another 0 to the number of repetitions
evalRec(repatedManyx)

val fact10 = fact(5)

evalRecSafe(fact10)