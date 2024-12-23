import fpinscala.exercises.datastructures.List
import fpinscala.exercises.datastructures.List.*

List("a", "b")
Cons("a", List("b"))
Cons("a", Cons("b", Nil))

List("a", "b") == Cons("a", List("b"))
List("a", "b") == Cons("a", Cons("b", Nil))

@annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
val result = List(1, 2, 3, 4, 5) match
  case Cons(x, Cons(2, Cons(4, _)))          => x
  case Nil                                   => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 1 + 2 = 3
  case Cons(h, t)                            => h + sum(t)
  case _                                     => 101

val l = List(0, 1, 2)

List.tail(l)

List.setHead(l, 3)

List.drop(l, 2)
List.drop(l, -2)

List.dropWhile(l, x => x < 2)
List.dropWhile(l, x => x < -2)
