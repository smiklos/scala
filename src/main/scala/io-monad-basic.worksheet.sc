import scala.annotation.tailrec
import scala.util.*


//introduce ref transparency and composability
// Basic stuff, real effect systems have 20+ branches
enum IO[+T] {
  case Pure(t: T)
  case Effect(t: () => T)

  case FlatMap[T, Y](io: IO[T], mapper: T => IO[Y]) extends IO[Y]

  def map[Y](mapper: T => Y): IO[Y] =
    FlatMap[T, Y](this, t => Effect(() => mapper(t)))

  def flatMap[Y](mapper: T => IO[Y]) = FlatMap[T, Y](this, mapper)

  def zip[Y](other: IO[Y]): IO[(T, Y)]  = this.flatMap(x => other.map(y => (x, y)))
}

object IO {

  def pure[T](t: T) = Pure(t)

  def effect[T](t: => T) = Effect(() => t)

  def suspend[T](t: => IO[T]) = Effect(() => t).flatMap(identity)

  def apply[T](t: => T) = effect(t)
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
  if num == 0 then IO.pure(1L) else IO.suspend(fact(num - 1)).map(_ * num)
}  

//Add another 0 to the number of repetitions
val repated10x = repeat(complexStuff, 10000)

eval(repated10x)


val repatedManyx = repeat(complexStuff, 10000)

//fix the stack overflow issue

evalRecSafe(repatedManyx)

val fact10 = fact(5)

evalRecSafe(fact10)

def blowup(num: Int) = IO.pure(num).map{x => 
  if(x % 2 == 0) {x / 2 } else {throw new Exception("Not divisible by 2!")} }


//error handling 
evalRecSafe(blowup(10))
evalRecSafe(blowup(11))

// can we manually fail the IO?

//use apply and zip 
evalRecSafe((IO("hi").zip(IO("there"))).map{case (a, b) => s"$a $b"})

// could now talk about adding an explicit failure operation and a recover option as well. Explain that its of course throwable based because of anticipating unknown errors.
// Alternative is to use either everywhere and encapsulate the error in the IO parameter itself
// ZIO is an alternative that does this.

// Effect systems are very invasive and take time to get used to. They are not always necessary but can be very useful in certain situations. 
//The best thing to do without effects systems is to use the Try monad and the Either monad and try to push the effects to the boundaries of the system (via dependency injection)


