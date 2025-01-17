import fpinscala.exercises.errorhandling.Option

val some              = Option.Some(0)
val none: Option[Int] = Option.None

some.map(_ + 1)
none.map(_ + 1)

some.getOrElse(1)
none.getOrElse(1)

some.flatMap(x => Option.Some(x + 1))
none.flatMap(x => Option.Some(x + 1))

some.orElse(Option.Some(1))
none.orElse(Option.Some(1))

some.filter(_ == 0)
some.filter(_ == 1)
none.filter(_ == 0)
none.filter(_ == 1)

Option.mean(List())
Option.mean(List(0, 1, 2, 3, 4))

Option.variance(List())
Option.variance(List(0, 1, 2, 3, 4))

Option.map2(some, some)(_ + _)
Option.map2(Option.Some(1), Option.Some(1))(_ + _)
Option.map2(Option.Some(2), Option.Some(2))(_ + _)

Option.map2(some, some)(_ * _)
Option.map2(Option.Some(1), Option.Some(1))(_ * _)
Option.map2(Option.Some(2), Option.Some(2))(_ * _)

Option.sequence(some :: Option.Some(1) :: Option.Some(2) :: Nil)
Option.sequence(none :: Option.Some(1) :: Option.Some(2) :: Nil)

Option.traverse(0 :: 1 :: 2 :: Nil)(
  a => if a == 0 then Option.None else Option.Some(a))
Option.traverse(0 :: 1 :: 2 :: Nil)(
  a => if a == 1 then Option.None else Option.Some(a))
Option.traverse(0 :: 1 :: 2 :: Nil)(
  a => if a == 2 then Option.None else Option.Some(a))
Option.traverse(0 :: 1 :: 2 :: Nil)(
  a => if a == 3 then Option.None else Option.Some(a))
