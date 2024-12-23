import fpinscala.exercises.datastructures.Tree
import fpinscala.exercises.datastructures.Tree.*

Branch(
  Branch(Leaf("a"), Leaf("b")),
  Branch(Leaf("c"), Leaf("d")),
)

val t = Branch(
          Branch(Leaf(0), Leaf(1)),
          Branch(Leaf(2), Leaf(3)),
        )

t.size

t.maximum

t.depth

t.map(_ + 1)

t.sizeViaFold

t.maximumViaFold

t.depthViaFold

t.mapViaFold(_ + 1)
