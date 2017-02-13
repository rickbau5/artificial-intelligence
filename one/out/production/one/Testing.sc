import scala.util.Try

val nums = 1 until 9 toList

val example = List(
    4, 1, 3,
    2, 5, 7,
    8, 6    )
val two = List(
    1, 2, 4,
    3, 5, 6,
    7, 8    )

val board = new Board(two.map(Option(_)) ++ List(Option.empty), 3)
board.show()

val b = new Board("13 425786".split("").map(c => Try(c.toInt).toOption).toList, 3)
b.show()
SolveEight.distance(b)
val c = new Board("1234 5786".split("").map(c => Try(c.toInt).toOption).toList, 3)
c.show()
SolveEight.distance(c)

c.hashCode()
c.##


// Math.abs(x1-x0) + Math.abs(y1-y0);

