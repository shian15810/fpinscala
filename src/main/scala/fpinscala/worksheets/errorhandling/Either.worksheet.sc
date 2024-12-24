import fpinscala.exercises.errorhandling.Either

val left: Either[Int, Int] = Either.Left(0)
val right: Either[Int, Int] = Either.Right(1)

left.map(_ + 1)
right.map(_ + 1)

left.flatMap(x => Either.Right(x + 1))
right.flatMap(x => Either.Right(x + 1))

left.orElse(Either.Right(2))
right.orElse(Either.Right(2))

left.map2(Either.Right(2))(_ + _)
right.map2(Either.Right(2))(_ + _)

left.map2(Either.Right(2))(_ * _)
right.map2(Either.Right(2))(_ * _)

Either.traverse(0 :: 1 :: 2 :: Nil)(a => if a == 0 then left else Either.Right(a))
Either.traverse(0 :: 1 :: 2 :: Nil)(a => if a == 1 then left else Either.Right(a))
Either.traverse(0 :: 1 :: 2 :: Nil)(a => if a == 2 then left else Either.Right(a))
Either.traverse(0 :: 1 :: 2 :: Nil)(a => if a == 3 then left else Either.Right(a))

Either.sequence(left :: Either.Right(1) :: Either.Right(2) :: Nil)
Either.sequence(right :: Either.Right(1) :: Either.Right(2) :: Nil)
