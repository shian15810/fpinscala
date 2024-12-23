import fpinscala.exercises.gettingstarted.{MyProgram, PolymorphicFunctions}

MyProgram.fib(0)
MyProgram.fib(1)
MyProgram.fib(2)
MyProgram.fib(3)
MyProgram.fib(4)
MyProgram.fib(5)
MyProgram.fib(6)
MyProgram.fib(7)
MyProgram.fib(8)
MyProgram.fib(9)

PolymorphicFunctions.isSorted(Array(1, 2, 3), _ > _)
PolymorphicFunctions.isSorted(Array(1, 2, 1), _ > _)
PolymorphicFunctions.isSorted(Array(3, 2, 1), _ < _)
PolymorphicFunctions.isSorted(Array(1, 2, 3), _ < _)
