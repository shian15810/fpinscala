import fpinscala.exercises.errorhandling.Option

val some = Option.Some(0)
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
