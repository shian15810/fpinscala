package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (rn, rng2) = rng.nextInt
    if rn < 0 then -(rn + 1) -> rng2 else rn -> rng2

  def double(rng: RNG): (Double, RNG) =
    val rn -> rng2 = nonNegativeInt(rng)
    rn / (Int.MaxValue.toDouble + 1) -> rng2

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(a => a / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val rn1 -> rng1 = rng.nextInt
    val rn2 -> rng2 = double(rng1)
    rn1 -> rn2 -> rng2

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val rn1 -> rng1 = rng.nextInt
    val rn2 -> rng2 = double(rng)
    rn2 -> rn1 -> rng2

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val rn1 -> rng1 = double(rng)
    val rn2 -> rng2 = double(rng1)
    val rn3 -> rng3 = double(rng2)
    (rn1, rn2, rn3) -> rng3

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 until count).foldRight(List[Int]() -> rng)((_, acc) =>
      val (rns, rng) = acc
      val rn1 -> rng1 = rng.nextInt
      (rns :+ rn1) -> rng1
    )

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng1 =>
    val rn1 -> rng2 = ra(rng1)
    val rn2 -> rng3 = rb(rng2)
    f(rn1, rn2) -> rng3

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List[A]()))((r, acc) => map2(r, acc)((a, as) => a :: as))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng0 =>
    val a -> rng1 = r(rng0)
    f(a)(rng1)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  def unit[S, A](a: A): State[S, A] = s => (a, s)

  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = s =>
      val (a, s1) = underlying(s)
      f(a)(s1)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
