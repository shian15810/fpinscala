import fpinscala.exercises.laziness.LazyList

import scala.util.Try

def if2Strict[A](cond: Boolean, onTrue: A, onFalse: A): A =
  if cond then onTrue else onFalse

Try(if2Strict(true, 1, sys.error("if2Strict")))

def if2Thunk[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
  if cond then onTrue() else onFalse()

if2Thunk(true, () => 1, () => sys.error("if2Thunk"))

/**
 * When defining a call-by-name argument using `onTrue => A` syntax, Scala
 * create a thunk (`() => A`) for `onTrue` automatically, essentially becoming
 * `() => onTrue`, and force the thunk (`onTrue()`) for each place `onTrue` is
 * referenced anywhere in the body of the function during function evaluation
 */
def if2NonStrict[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if cond then onTrue else onFalse

if2NonStrict(true, 1, sys.error("if2Strict"))

def maybeTwice2(b: Boolean, i: => Int): Int =
  lazy val j = i
  if b then j + j else 0

maybeTwice2(true, { println("hi"); 1 + 41 })

val ll = LazyList(0, 1, 2)
ll.toList

ll.take(-1).toList
ll.take(0).toList
ll.take(1).toList
ll.take(2).toList
ll.take(3).toList
ll.take(4).toList

ll.drop(-1).toList
ll.drop(0).toList
ll.drop(1).toList
ll.drop(2).toList
ll.drop(3).toList
ll.drop(4).toList

ll.takeWhile(n => n < -1).toList
ll.takeWhile(n => n < 0).toList
ll.takeWhile(n => n < 1).toList
ll.takeWhile(n => n < 2).toList
ll.takeWhile(n => n < 3).toList
ll.takeWhile(n => n < 4).toList

val ones: LazyList[Int] = LazyList.cons(1, ones)

ones.take(5).toList
ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.takeWhile(_ == 1).take(5).toList
ones.forAll(_ != 1)

LazyList.continually(0).take(5).toList

LazyList.from(0).take(5).toList

def fib(n: Int): Int =
  def go(m: Int, curr: Int, next: Int): Int =
    if m == n then curr
    else go(m + 1, next, curr + next)
  go(0, 0, 1)

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)
fib(7)
fib(8)
fib(9)

lazy val fibs: LazyList[Int] =
  lazy val num = LazyList.from(0).map(fib)
  num

fibs.take(10).toList
LazyList.fibs.take(10).toList
LazyList.fibsViaUnfold.take(10).toList

LazyList.fromViaUnfold(10).take(10).toList

LazyList.continuallyViaUnfold(5).take(10).toList

LazyList.onesViaUnfold.take(10).toList
