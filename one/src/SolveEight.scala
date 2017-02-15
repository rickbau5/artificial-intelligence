import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Random, Try}

/**
  * Project #1: Solve Eight
  * CS470/570 Artificial Intelligence
  * Terence Soule
  *
  * Author: Richard Boss
  * Date: 2 Feb. 2017
  */
object SolveEight {
    import Action.No
    val rand = new Random(42 + 666)
    val DFS_MAX_DEPTH = 31

    def main(args: Array[String]): Unit = {
        go(args)
    }

    def go(args: Array[String]): Unit = {
        val function = Try(args(0) match {
            case "bfs" =>
                println("Using Breadth First Search without closed list")
                (board: Board) => {
                    solve(board) { b =>
                        val start = new Node(b, No, None, 0)
                        bfs_Stream(start.children)
                    }
                }
            case "bfs-closed" =>
                println("Using Breadth First Search with closed list")
                (board: Board) => {
                    solve(board) { b =>
                        val start = new Node(b, No, None, 0)
                        bfs_Closed(start.children.toSet, HashSet(start))
                    }
                }
            case "dfs" =>
                println("Using Depth First Search with closed list")
                (board: Board) => {
                    solve(board) { b =>
                        val start = new Node(b, No, None, 0)
                        dfs(start.children.toList, HashSet(start))
                    }
                }
            case "a*-h1" =>
                println("Using A* with 'misplaced' heuristic with closed list.")
                (board: Board) => {
                    solve(board) { b =>
                        val start = new Node(b, No, None, 0)
                        aStar(
                            start.children.map(n => (n, misplaced(n.state.asInstanceOf[Board]))).toList, HashSet(start)
                        )(misplaced)
                    }
                }
            case "a*-h2" =>
                println("Using A* with 'distance' heuristic with closed list.")
                (board: Board) => {
                    solve(board) { b =>
                        val start = new Node(b, No, None, 0)
                        aStar(
                            start.children.map(n => (n, distance(n.state.asInstanceOf[Board]))).toList, HashSet(start)
                        )(distance)
                    }
                }
        }).getOrElse((board: Board) => { solve(board) { b =>
            val start = new Node(b, No, None, 0)
            aStar(
                start.children.map(n => (n, distance(n.state.asInstanceOf[Board]))).toList, HashSet(start)
            )(distance)
        }})

        val iterations = Try(args(1).toInt).getOrElse(30)
        val boardSize  = Try(args(2).toInt).getOrElse(3)

        run(iterations, boardSize)(function)
    }

    def timed[B](func: => B): (B, Long) = {
        val start = System.currentTimeMillis()
        val ret = func
        val end = System.currentTimeMillis()
        (ret, end - start)
    }

    def run(iterations: Int, size: Int)(solveFunction: (Board) => List[Node]): Unit = {
        (1 to iterations) map { it =>
            val board = Board.shuffle(Board.generateBoard(size), 100)
            println(s"Beginning iteration $it.")
            board.show()
            val (solution, time) = timed(solveFunction(board))
            if (solution.nonEmpty) {
                println(s"Found solution in ${time / 1000.0} seconds.")
                board.cells.flatMap(_.map(_.toString).getOrElse(" "))
                  .sliding(size, size)
                  .zip(
                    solution.last.state.asInstanceOf[Board].cells
                      .flatMap(_.map(_.toString).getOrElse(" "))
                      .sliding(size, size)
                  ).foreach(p => println(s"${p._1.mkString(" ")}   ${p._2.mkString(" ")}"))
                println("Number of moves: " + solution.length)
                println(s"# ${time / 1000.0} ${solution.length} ${Node.generated} ${Node.visited}")
            } else {
                println("Failed to find a solution")
                println(s"# ${time / 1000.0} -1 ${Node.generated} ${Node.visited}")
            }
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
    def bfs_Stream(layer: Stream[Node]): Option[Node] = layer.find(_.state.isGoal) match {
        case s: Some[Node] => s
        case None =>
            Node.visited += layer.size
            bfs_Stream(layer.flatMap(_.children))
    }

    @tailrec
    def bfs_Closed(layer: Set[Node], visited: HashSet[Node]): Option[Node] = layer.find(_.state.isGoal) match {
      case s: Some[Node] => s
        case None =>
            Node.visited += layer.size
            val vis = visited ++ layer
            bfs_Closed(layer.flatMap(_.children) -- vis, vis)
    }

    def dfs(nodes: List[Node], visited: HashSet[Node], depth: Int = 0): Option[Node] = nodes match {
        case head :: tail =>
            Node.visited += 1
            if (head.state.isGoal) {
                Some(head)
            } else if (visited.contains(head) || depth > DFS_MAX_DEPTH) {
                None
            } else {
                val sol = dfs(head.children.toList, visited + head, depth + 1)
                if (sol.nonEmpty) {
                    sol
                } else {
                    dfs(tail, visited + head, depth + 1)
                }
            }
        case _ =>
            None
    }

    def misplaced(board: Board): Int = {
        board.cells.zipWithIndex
          .map {
              case (Some(v), i) => v - 1 != i
              case (None, i)    => i != 8
          }.count(_ == true)
    }

    /**
      * Compute the sum of all the Manhattan distances between where the cells are and
      * where they should be.
      *
      * @param board
      * @return
      */
    def distance(board: Board): Int = {
        val w = board.width
        board.cells.zipWithIndex
          .map {
              case (Some(v), i) =>
                  // println(v, i + 1, (v - 1) % w, (v - 1) / w, i % w, i / w)
                  Math.abs((v - 1) % w - i % w) + Math.abs((v - 1) / w - i / w)
              case (None, _) =>
                  0
          }.sum
    }

    @tailrec
    def aStar(nodes: List[(Node, Int)], visited: HashSet[Node])(heuristic: (Board) => Int): Option[Node] = nodes match {
        case (node, cost) :: rest =>
            Node.visited += 1
            if (node.state.isGoal) {
                Some(node)
            } else if (visited.contains(node)) {
                aStar(rest, visited)(heuristic)
            } else {
                val next = (node.children.map(n => (n, heuristic(n.state.asInstanceOf[Board]) + cost)).toList ++ rest)
                  .sortBy(_._2)
                aStar(next, visited + node)(heuristic)
            }

        case Nil =>
            null
    }

    def solve[A](start: A)(recursiveFunction: (A) => Option[Node]): List[Node] = {
        Node.generated = 0
        Node.visited = 0
        val solutionNode = recursiveFunction(start)

        solutionNode.map(generateMoves)
          .getOrElse(List.empty[Node])
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
    import Action._
    val intRep = cells.map(_.getOrElse(0)).mkString("").toInt
    /**
      * Check if board is solved.
      *
      * @return T/F whether board is solved
      */
    override def isGoal: Boolean = {
        intRep == 123456780
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
                // Basically just a swap, where there is no swap for List
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
            // other.cells.equals(this.cells)
            this.intRep == other.intRep
        case _ => false
    }

    override def hashCode(): Int = {
        intRep
    }
}

object Action {
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
}

sealed trait Action {
    // Move that undoes this one
    val inverse: Action
    // For ordering
    val id: Int
}

object Node {
    var generated = 0
    var visited = 0
}
class Node(val state: State, val lastMove: Action, val parent: Option[Node], val depth: Int) {
    Node.generated += 1

    lazy val children = state.validActions.toStream
      .filter(_ != lastMove.inverse)
      .map(action => new Node(state.transition(action), action, Option(this), depth + 1))

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

    override def hashCode(): Int = state.hashCode()
}
