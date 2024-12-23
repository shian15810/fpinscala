import fpinscala.exercises.datastructures.List
import fpinscala.exercises.datastructures.List.*

List("a", "b")
Cons("a", List("b"))
Cons("a", Cons("b", Nil))

List() == Nil
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

List.init(l)

List.length(l)

List.sumViaFoldLeft(l)

List.productViaFoldLeft(List(0.0, 1.0, 2.0))
List.productViaFoldLeft(List(1.0, 2.0, 3.0))

List.lengthViaFoldLeft(l)

List.reverse(l)

List.appendViaFoldRight(l, List(3))

List.concat(List(l, List(3, 4, 5)))

List.incrementEach(l)

List.doubleToString(List(0.0, 1.0, 2.0))

List.map(l, x => x + 1)

List.filter(l, x => x % 2 == 0)

List.flatMap(l, i => List(i, i))

List.filterViaFlatMap(l, x => x % 2 == 0)

List.addPairwise(l, List(3, 4, 5))

List.zipWith(l, List(3, 4, 5), (x, y) => (x, y))
