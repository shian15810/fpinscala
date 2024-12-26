package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case Cons(_, _) if n <= 0 => empty
    case z => z

  def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case z => z

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if p(a) then cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: (A => B)): LazyList[B] =
    foldRight(empty)((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty)((a, b) => if f(a) then cons(a, b) else b)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: (A => LazyList[B])): LazyList[B] =
    foldRight(empty)((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this):
    case Cons(h, t) => Some(f(h()), t())
    case _ => None

  def takeViaUnfold(n: Int): LazyList[A] = unfold((this, n)):
    case (Cons(h, t), 1) => Some(h(), (empty, 1))
    case (Cons(h, t), m) if n > 1 => Some(h(), (t(), m - 1))
    case _ => None

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] = unfold(this):
    case Cons(h, t) if f(h()) => Some(h(), t())
    case _ => None

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this, that):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(a => a._2.isDefined).forAll((a1, a2) => a1 == a2)

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case Cons(h, t) => Some(Cons(h, t), t())
      case _ => None
    .append(LazyList(Empty))

  def hasSubsequence[A](l: LazyList[A]): Boolean =
    tails.exists(_.startsWith(l))


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    lazy val as: LazyList[A] = cons(a, as)
    as

  def from(n: Int): LazyList[Int] =
    lazy val ns: LazyList[Int] = cons(n, from(n + 1))
    ns

  def fib(n: Int) =
    def loop(m: Int, curr: Int, next: Int): Int =
      if m == n then curr
      else loop(m + 1, next, curr + next)
    loop(0, 0, 1)

  lazy val fibs: LazyList[Int] =
    lazy val s = from(0).map(fib)
    s

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => empty
      case Some(a, s) => cons(a, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] = unfold(0)(s => Some((fib(s), s + 1)))

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(s => Some(s, s + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some((a), ()))

  lazy val onesViaUnfold: LazyList[Int] = unfold(())(_ => Some(1, ()))
