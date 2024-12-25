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
