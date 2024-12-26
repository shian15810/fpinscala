import fpinscala.exercises.state.RNG

Int.MaxValue
Int.MinValue

val rng = RNG.Simple(42)

val (n1, rng1) = rng.nextInt
val (n2, rng2) = rng1.nextInt

RNG.nonNegativeInt(rng)

RNG.double(rng)

RNG.intDouble(rng)
RNG.doubleInt(rng)
RNG.double3(rng)