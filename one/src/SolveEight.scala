import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.util.Random

/**
  * Project #1: Solve Eight
  * CS470/570 Artificial Intelligence
  * Terence Soule
  *
  * Author: Richard Boss
  * Date: 2 Feb. 2017
  */
object SolveEight {
    val rand = new Random()
    val DFS_MAX_DEPTH = 31

    def main(args: Array[String]): Unit = {
        val board = Board.generateBoard(3).transition(Down).transition(Right).transition(Down).transition(Right)

        board.show()
        val start = new Node(board, No, None, 0)
        val (bfsSol, bfsTime) = timed(solve(start)(node => bfs_Stream(node.children.toStream)))
        println(bfsSol.length)
        println(bfsTime / 1000.0)
        println(Node.visited)
        println("-----")
        val (dfsSol, dfsTime) = timed(solve(start)(node => dfs(node.children, List(node))))
        println(dfsSol.length)
        println(dfsTime / 1000.0)
        println(Node.visited)
        // run(30, 3)(solve(_) { board =>
        //     bfs_Stream(new Node(board, No, None).children.toStream)
        // })
    }

    def timed[B](func: => B): (B, Long) = {
        val start = System.currentTimeMillis()
        val ret = func
        val end = System.currentTimeMillis()
        (ret, end - start)
    }

    def run(iterations: Int, size: Int)(solveFunction: (Board) => List[Node]): Unit = {
        (0 until iterations) map { it =>
            val board = Board.shuffle(Board.generateBoard(size), 75)
            println(s"Beginning iteration $it.")
            board.show()
            val (solution, time) = timed(solveFunction(board))
            println(s"Found solution in ${time / 1000.0} seconds.")
            board.cells.flatMap(_.map(_.toString).getOrElse(" "))
              .sliding(size, size)
              .zip(
                  solution.last.state.asInstanceOf[Board].cells
                    .flatMap(_.map(_.toString).getOrElse(" "))
                    .sliding(size, size)
              ).foreach(p => println(s"${p._1.mkString(" ")}   ${p._2.mkString(" ")}"))
            println("Number of moves: " + solution.length)
            println(s"# ${time / 1000.0} ${solution.length} ${Node.visited}")
            println("--------------------------")
            solution
        }
    }

    def generateMoves(endNode: Node): List[Node] = {
        val moves = mutable.ListBuffer.empty[Node]
        var node = endNode
        while (node.parent.isDefined) {
            moves += node
            node = node.parent.get
        }
        moves.reverse.toList
    }

    @tailrec
    def bfs_Stream(layer: Stream[Node]): Node = layer.find(_.state.isGoal) match {
        case Some(n) => n
        case None =>
            bfs_Stream(layer.flatMap(_.children))
    }

    @tailrec
    def bfs_Closed(layer: Set[Node], visited: mutable.ListBuffer[Node]): Node = layer.find(_.state.isGoal) match {
        case Some(n) => n
        case None =>
            visited ++= layer
            bfs_Closed(layer.flatMap(_.children) -- visited, visited)
    }

    @tailrec
    def dfs(nodes: List[Node], visited: List[Node]): Node = nodes match {
        case node :: rest =>
            if (node.state.isGoal) {
                node
            } else if (visited.contains(node) || node.depth == DFS_MAX_DEPTH) {
                dfs(rest, visited)
            } else {
                dfs(node.children ++ rest, visited ++ List(node))
            }
        case Nil =>
            null
    }

    def solve[A](start: A)(recursiveFunction: (A) => Node): List[Node] = {
        Node.visited = 0
        val solutionNode = recursiveFunction(start)

        generateMoves(solutionNode)
    }

    def playSolution(solution: List[Node]): Unit = {
        solution foreach { node =>
            println(node.state)
            println(node.lastMove)
            StdIn.readLine("> ")
        }
    }
}

abstract class State {
    def isGoal: Boolean
    def transition(action: Action): State
    def validActions: List[Action]
}
object Board {
    def default = generateBoard(3)

    /**
      * Generate a puzzle of size n with the empty spot at n * n
      * @param size
      * @return a new instance of Board
      */
    def generateBoard(size: Int): Board = {
        val cells = (1 until (size * size)).map(Option(_)) ++ List(None)
        Board(cells.toList, size)
    }

    /**
      * Shuffle the board by performing n random legal moves.
      * @param n
      * @return this, now shuffled
      */
    def shuffle(board: Board, n: Int): Board = {
        (0 until n).foldLeft(board) { case (b, _) =>
            val cellNeighbors = b.neighbors(b.findEmpty()._1).map(_._1)
            val pick = cellNeighbors(SolveEight.rand.nextInt(cellNeighbors.length))
            b.move(pick)
        }
    }
}
case class Board(cells: List[Option[Int]], width: Int) extends State {
    /**
      * Check if board is solved.
      *
      * @return T/F whether board is solved
      */
    override def isGoal: Boolean = {
        // this way is a bit faster than old method of checking
        var last = cells.flatten.head
        var ordered = true
        for (i <- cells.flatten.tail if ordered) {
            if (i < last) {
                ordered = false
            }
            last = i
        }
        ordered
    }

    // Provide logic for board(1) where board is instance
    def apply(i: Int): Option[Int] = cells(i)

    /**
      * Attempt to move the piece at the given index. Will either slide to the
      * neighboring empty space, or do nothing.
      *
      * @param index
      * @return T/F whether a move was made
      */
    def move(index: Int): Board = {
        neighbors(index).find(_._2.isEmpty)  match { // Get neighbors and find the empty one
            case Some((idx, cell)) =>
                val (mI, mC, xI, xC) = if (index < idx) {
                    (index, cell, idx, cells(index))
                } else {
                    (idx, cells(index), index, cell)
                }
                // Rebuild the board by slicing up the board based on the indices of the moving piece and empty.
                // Basically just a swap, where there is no swap for ListBuffer
                val newCells = cells.slice(0, mI) ++ List(mC) ++
                  cells.slice(mI + 1, xI) ++ List(xC) ++ cells.slice(xI + 1, cells.length)
                Board(newCells, width)
            case None =>
                this
        }
    }

    def transition(dir: Action): Board = {
        val (idx, _) = findEmpty()
        val moveIdx = dir match {
            case Left => right(idx)
            case Right => left(idx)
            case Up => down(idx)
            case Down => up(idx)
            case No =>
                throw new IllegalStateException("No shouldn't be accessible.\n" + toString)
        }
        moveIdx match {
            case Some(index) =>
                move(index)
            case None =>
                this
        }
    }

    def validActions: List[Action] = {
        val (idx, _) = findEmpty()
        val moves = List(
            left(idx).map(_ => Right),
            right(idx).map(_ => Left),
            up(idx).map(_ => Down),
            down(idx).map(_ => Up)
        )
        moves.flatten
    }

    /**
      * At the given index, use the provided predicate to decide whether to
      * apply the function mutator.
      *
      * @param index
      * @param predicate Some test whether to operate on the given int
      * @param mutator The function to modify the given index
      * @return Either:
      *         Some(modified Index) <= predicate true
      *         None                 <= predicate false
      */
    def neighborIndexOrNone(index: Int, predicate: (Int) => Boolean, mutator: (Int) => Int) : Option[Int] = predicate(index) match {
        case true  => Option(mutator(index))
        case false => None
    }

    // Several functions to compute the neighboring cell, or none if on an edge.
    def left(index: Int): Option[Int] = neighborIndexOrNone(index, _ % width != 0, _ - 1)
    def right(index: Int): Option[Int] = neighborIndexOrNone(index, _ % width != width - 1, _ + 1)
    def up(index: Int): Option[Int] = neighborIndexOrNone(index, _ - width >= 0, _ - width)
    def down(index: Int): Option[Int] = neighborIndexOrNone(index, _ + width < width * width, _ + width)

    /**
      * Takes a cell's index and returns neighboring cells
      * @param index
      * @return
      */
    def neighbors(index: Int): List[(Int, Option[Int])] = {
        List(left(index), right(index), up(index), down(index))
          .flatten
          .map(idx => idx -> cells(idx))
    }

    def findEmpty(): (Int, Option[Int]) = cells.zipWithIndex.find(_._1.isEmpty).get.swap

    def show(): Unit = println(this)

    override def toString: String = {
        var (spacing, count) = (0, width * width)
        while (count > 0) {
            count /= 10
            spacing += 1
        }
        cells.map(_.map(_.toString).getOrElse(" "))
          .sliding(width, width)
          .map { row =>
              row.map(entry => s"$entry" + List.fill(spacing - entry.length + 1)(" ").mkString)
                .mkString("")
          }
          .mkString("\n")
    }

    override def equals(obj: scala.Any): Boolean = obj match {
        case other: Board =>
            other.cells.equals(this.cells)
        case _ => false
    }
}

sealed trait Action {
    // Move that undoes this one
    val inverse: Action
    // For ordering
    val id: Int
}
case object Left extends Action {
    override val inverse = Right
    override val id = 0
}
case object Right extends Action {
    override val inverse = Left
    override val id = 1
}
case object Up extends Action {
    override val inverse = Down
    override val id = 2
}
case object Down extends Action {
    override val inverse = Up
    override val id = 3
}
case object No extends Action {
    override val inverse = No
    override val id = 4
}

object Node {
    var visited = 0
}
class Node(val state: State, val lastMove: Action, val parent: Option[Node], val depth: Int) {
    Node.visited += 1
    // Only evaluate children when asked for
    lazy val children = state.validActions
      .filter(_ != lastMove.inverse)
      .map(action => new Node(state.transition(action), action, Option(this), depth + 1))
      .distinct
      .sortBy(_.lastMove.id)

    override def toString: String = {
        "Node [\nboard: \n" +
          state + "\nlast:\n" +
          lastMove +
          "\n]"
    }

    override def equals(obj: scala.Any): Boolean = obj match {
        case other: Node => other.state.equals(this.state)
        case _ => false
    }
}
