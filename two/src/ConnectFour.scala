import java.io.FileOutputStream

import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Failure, Random, Success, Try}
import collection.JavaConverters._
import scala.reflect.ClassTag

/**
  * Created by Rick on 2/22/2017.
  */
object ConnectFour {

    var DEBUG_ENABLED = false

    def getInput[A](prompt: String = "")(partialFunction: PartialFunction[String, A]): A = Try {
        val input = StdIn.readLine(prompt)
        partialFunction.apply(input)
    }.getOrElse(getInput("Unrecognized input.\n" + prompt)(partialFunction))

    val choiceR = raw"([0-9])".r
    val scoreR = raw"s ([12])".r
    val fileR = raw"file (.*)".r

    def playWithAi(board: ConnectBoard, player: Int, aiPlayer: Int): Unit = {
        var _aiPlay = true
        var rounds = 0
        while (_aiPlay) {
            println(s"Round $rounds.")
            def _play(pl: Int): Int = {
                val move = minmax_AlphaBeta(board, pl, 6)(evaluationFunction)
                board.place(move, pl)
                board.show()
                board.victor._1
            }
            val pA = _play(player)
            if (pA != 0) {
                println(s"Player ${if (player == 1) "o" else "x"} wins!")
                _aiPlay = false
            } else {
                val pB = _play(aiPlayer)
                if (pB != 0) {
                    println(s"Player ${if (aiPlayer == 1) "o" else "x"} wins!")
                    _aiPlay = false
                }
            }
            rounds += 1
        }

        println("Number of rounds until win: " + rounds)
    }

    def main(args: Array[String]): Unit = {
        val board = new ConnectBoard(7, 6)

        println(
            """Welcome to Connect Four!
              | Select the player that will go first (1, 2)
            """.stripMargin)
        val player = getInput("Player: ") {
            case "1" => 1
            case "2" => 2
        }
        println(s"Player $player (${if (player == 1) "o" else "x"}) will be controlled by you and go first")
        println(
            """ Enter a number 0-6 indicating the column to place the piece.
              | 0 is the furthest left column, 6 is the furthest right.
              | Have fun! And good luck against Al!
              | Press enter to begin playing.
            """.stripMargin)

        StdIn.readLine()

        board.show()

        var play = true

        val aiPlayer = 1 + player % 2

        do {
            val playerMoved = getInput("> ") {
                case choiceR(numStr) =>
                    val choice = numStr.toInt
                    val placed = board.place(choice, player)
                    if (!placed) {
                        println("Column full, try again.")
                        false
                    } else {
                        true
                    }

                case "show" =>
                    board.show()
                    false

                case scoreR(str) =>
                    val playerToScore = Try(str.trim.toInt).getOrElse(board.nextPlayer)
                    val score = board.score(playerToScore)
                    println("Score for " + (if (playerToScore == 1) "o" else "x"))
                    score.sliding(7, 7)
                      .foreach(line => println(line.map(e => "%.1f".format(e)).mkString(" ")))
                    false

                case "play" =>
                    playWithAi(board, player, aiPlayer)

                    println("Bye!")
                    sys.exit(0)

                case fileR(fileName) =>
                    Try(new FileOutputStream(fileName)) match {
                        case Success(fos) =>
                            println(s"Successfully opened $fileName, beginning playing.")
                            Console.withOut(fos) {
                                playWithAi(board, player, aiPlayer)
                            }
                            println("Done.")
                            println("Bye!")
                            sys.exit(0)

                        case Failure(ex) =>
                            ex.printStackTrace()
                            false
                    }

                case "debug" =>
                    DEBUG_ENABLED = !DEBUG_ENABLED
                    if (DEBUG_ENABLED) {
                        println("Debug mode enabled.")
                    } else {
                        println("Debug mode disabled.")
                    }
                    false

                case "quit" =>
                    println("Bye!")
                    play = false
                    false
            }

            if (playerMoved) {
                board.show()
                val (victor, _) = board.victor
                if (victor != 0) {
                    println("Congratulations, you win!")
                    play = false
                } else {
                    println("Al's turn.")
                    val move = minmax_AlphaBeta(board, aiPlayer, 6) { case (b, pl) =>
                        val ret = b.score(pl).max
                        if (pl != b.lastPlayer) {
                            -ret
                        } else {
                            ret
                        }
                    }

                    board.place(move, aiPlayer)
                    board.show()

                    val (victor, _) = board.victor
                    if (victor != 0) {
                        println("Sorry, you lost! Al wins this time")
                        play = false
                    }
                }
            }
        } while (play)
    }

    def evaluationFunction(board: ConnectBoard, player: Int ): Double = {
        val ret = board.score(player).max
        if (player != board.lastPlayer) {
            -ret
        } else {
            ret
        }
    }

    def utility(board: ConnectBoard, player: Int): Double = {
        val victor = board.victor
        val util = victor match {
            case (0, _) => 0.0
            case (pl, _) if pl == player =>
                1.0
            case (pl, _) if pl != player =>
                -1.0
            case _ =>
                0.0
        }
        util
    }

    /**
      * Minmax algorithm with Alpha-Beta pruning rolled in. This takes a given board,
      * the current player, the number of moves to look ahead and a utility/evaluation
      * function. Will return the best move to make given these parameters.
      *
      * It takes into account blocking an immediate winning move of the opponent, and
      * taking an immediate winning move for the current player if multiple paths to
      * wins exist.
      *
      * @param start the starting board
      * @param player the player
      * @param lookAhead number of "moves" to look ahead
      * @param func utility/evaluation function (ConnectBoard, Int) => Double
      * @return the row to place the next piece in
      */
    def minmax_AlphaBeta(start: ConnectBoard, player: Int, lookAhead: Int)(func: (ConnectBoard, Int) => Double): Int = {
        var NODES_EXAMINED = 0
        var BRANCHES_PRUNED = 0
        var BRANCHES = 0L
        var LEVELS = 0L
        def max(node: Node[ConnectBoard], _alpha: Double, beta: Double)(depth: Int): Double = {
            NODES_EXAMINED += 1
            var alpha = _alpha
            val next = node.state.next
              //.sortBy(board => board.score(player).max)
              //.reverse

            BRANCHES += next.length.toLong
            LEVELS += 1L

            if (depth == 0 || next.isEmpty || node.state.victor._1 != 0) {
                val util = func(node.state, player)
                util
            } else {
                var v = Double.NegativeInfinity
                next foreach { child =>
                    val res = min(new Node(child, Option(node)), alpha, beta)(depth - 1)
                    if (res > v) {
                        v = res
                    }
                    if (v >= beta) {
                        return v
                    }
                    alpha = if (alpha > v) {
                        BRANCHES_PRUNED += 1
                        alpha
                    } else {
                        v
                    }

                }
                v
            }
        }

        def min(node: Node[ConnectBoard], alpha: Double, _beta: Double)(depth: Int): Double = {
            NODES_EXAMINED += 1
            var beta = _beta
            val next = node.state.next
              //.sortBy(board => board.score(1 + player % 2).max)

            BRANCHES += next.length.toLong
            LEVELS += 1L

            if (depth == 0 || next.isEmpty || node.state.victor._1 != 0) {
                val util = func(node.state, player)
                util
            } else {
                var v = Double.PositiveInfinity
                next foreach { child =>
                    val res = max(new Node(child, Option(node)), alpha, beta)(depth - 1)
                    if (res < v) {
                        v = res
                    }
                    if (v <= alpha) {
                        return v
                    }
                    beta = if (beta < v) {
                        BRANCHES_PRUNED += 1
                        beta
                    } else v
                }
                v
            }
        }

        val rootNode = new Node(start, None)

        val startMs = System.currentTimeMillis()
        val results = start.nextWithAction.map { case (move, board) =>
            (move, min(new Node(board, Option(rootNode)), Double.NegativeInfinity, Double.PositiveInfinity)(lookAhead), board)
        }.sortBy(_._1)
        val endMs = System.currentTimeMillis()

        val top = results.maxBy(_._2)
        val tiedResults = results.filter(_._2 == top._2)
        val choice = tiedResults(Random.nextInt(tiedResults.length))
          ._1

        val winningMove = results.map(_._3.victor)
          .find(_._1 == player)
          .map(_._2 % start.w)

        val nextPlayerMoveResults = start.validPlaces(1 + player % 2).zipWithIndex
          .filter(_._1 != 0.0)
          .map { case (pl, idx) =>
              val board = start.copy()
              board.place(idx % board.w, pl)
              board.victor
          }

        val blockingMove = nextPlayerMoveResults.find(_._1 != 0)
            .map(_._2 % start.w)

        debug {
            println("Time elapsed: %.3f".format((endMs - startMs) / 1000.0))
            println(s"Examined $NODES_EXAMINED nodes.")
            println(s"Pruned $BRANCHES_PRUNED branches.")
            println("Average branching factor (without pruning): %.2f".format(BRANCHES / LEVELS.toDouble))

            print("Move scores:\n ")
            results foreach (tup => print("%.1f ".format(tup._2)))
            println

            println("Next winning moves for both sides:")

            val myWinningMoves = results.map(_._3.victor)
              .map { case (w, i) => (w, i % start.w) }
            val opponentWinningMoves = nextPlayerMoveResults.map(tup => (tup._1, tup._2 % start.w))

            (myWinningMoves ++ opponentWinningMoves).groupBy(_._2)
              .map { case (idx, list) => idx -> list.map(_._1) }
              .toList.sortBy(_._1)
              .foreach { case (idx, wins) =>
                  println(s"$idx -> ${wins.mkString(",")}")
              }

            println("Algo: " + choice)
            println("Winning: " + winningMove)
            println("Blocking: " + blockingMove)
        }

        /* Take the winning move first if there is, then blocking move, then algo's choice */
        winningMove.getOrElse(blockingMove.getOrElse(choice))
    }

    /**
      * Execute 'func' if debug is enabled
      *
      * @param func
      * @tparam A
      * @return
      */
    def debug[A](func: => A): Any = if (DEBUG_ENABLED) func
}

trait State {
    def next: List[State]
}
class ConnectBoard(val w: Int, val h: Int, val dim: Int = 4) extends State {
    private var _cachedScore = Option.empty[List[Double]]
    def isScoreCached = _cachedScore.isDefined

    val list = new java.util.ArrayList[Integer](w * h)
    (0 until w * h).foreach(_ => list.add(0))

    var lastMoveIndex = Option.empty[Int]
    var lastPlayer = 2

    def indexToXY(idx: Int): (Int, Int) = (idx % w, idx / w)
    def xyToIndex(xy: (Int, Int)): Int = xy._2 * w + xy._1

    def nextPlayer = 1 + lastPlayer % 2

    /**
      * Return the winner for the board.
      *
      * @return (winning player [0, 1, 2], index)
      */
    def victor: (Int, Int) = lastMoveIndex match {
        case Some(last) =>
            var l = true
            var r = true
            var u = true
            var d = true
            var b = true
            var f = true
            var bb = true
            var ff = true

            var sum_r = 1
            var sum_c = 1
            var sum_d1 = 1
            var sum_d2 = 1

            for (i <- 1 to 3 if sum_r < dim & sum_c < dim & sum_d1 < dim & sum_d2 < dim) {
                val xy = indexToXY(last)
                if (l) {
                    if (xy._1 - i >= 0) {
                        val v = list.get(xyToIndex(xy._1 - i, xy._2))
                        if (v == lastPlayer) {
                            sum_r += 1
                        } else {
                            l = false
                        }
                    } else {
                        l = false
                    }
                }
                if (r) {
                    if (xy._1 + i < w) {
                        val v = list.get(xyToIndex(xy._1 + i, xy._2))
                        if (v == lastPlayer) {
                            sum_r += 1
                        } else {
                            r = false
                        }
                    } else {
                        r = false
                    }
                }
                if (u) {
                    val xy2 = xyToIndex(xy._1, xy._2 - i)
                    if (xy2 >= 0 && list.get(xy2) == lastPlayer) {
                        sum_c += 1
                    } else {
                        u = false
                    }
                }
                if (d) {
                    val xy2 = xyToIndex(xy._1, xy._2 + i)
                    if (xy2 < w * h && list.get(xy2) == lastPlayer) {
                        sum_c += 1
                    } else {
                        d = false
                    }
                }
                if (b) {
                    if (xy._1 - i >= 0 && xy._2 - i >= 0) {
                        if (list.get(xyToIndex(xy._1 - i, xy._2 - i)) == lastPlayer) {
                            sum_d1 += 1
                        } else {
                            b = false
                        }
                    } else {
                        b = false
                    }
                }
                if (f) {
                    if (xy._1 + i < w && xy._2 + i < h) {
                        if (list.get(xyToIndex(xy._1 + i, xy._2 + i)) == lastPlayer) {
                            sum_d1 += 1
                        } else {
                            f = false
                        }
                    } else {
                        f = false
                    }
                }
                if (bb) {
                    if (xy._1 - i >= 0 && xy._2 + i < h) {
                        if (list.get(xyToIndex(xy._1 - i, xy._2 + i)) == lastPlayer) {
                            sum_d2 += 1
                        } else {
                            bb = false
                        }
                    } else {
                        bb = false
                    }
                }
                if (ff) {
                    if (xy._1 + i < w && xy._2 - i >= 0) {
                        if (list.get(xyToIndex(xy._1 + i, xy._2 - i)) == lastPlayer) {
                            sum_d2 += 1
                        } else {
                            ff = false
                        }
                    } else {
                        ff = false
                    }
                }
            }

            if (sum_r >= 4) {
                (lastPlayer, last)
            } else if (sum_c >= 4) {
                (lastPlayer, last)
            } else if (sum_d1 >= 4 || sum_d2 >= 4) {
                (lastPlayer, last)
            } else {
                (0, last)
            }
        case None =>
            (0, 0)
    }

    /**
      * Return the valid places where a piece can be.
      *
      * @param player
      * @return
      */
    def validPlaces(player: Int): List[Int] = {
        val map = list.asScala.zipWithIndex.map(_.swap).toMap
        list.asScala.zipWithIndex
          .foldLeft(List.empty[Int]) { case (ls, (pl, index)) =>
              if (pl == 0) {
                  ls ++ (map.get(index + w) match {
                      case Some(below) if below != 0 => List(player)
                      case None => List(player)
                      case _ => List(0)
                  })
              } else {
                  ls ++ List(0)
              }
          }
    }

    def score(playerToScore: Int = nextPlayer): List[Double] = {
        if (_cachedScore.nonEmpty) {
            return _cachedScore.get
        }
        val map = list.asScala.zipWithIndex.map(_.swap).toMap

        val scores = validPlaces(playerToScore).zipWithIndex
          .map { case (player, index) =>
              if (player == playerToScore) {
                  val xy = indexToXY(index)
                  var l = true
                  var r = true
                  var d = true
                  var u = true
                  var b = true
                  var f = true
                  var bb = true
                  var ff = true

                  var sum_r = 0.0
                  var sum_c = 0.0
                  var sum_d1 = 0.0
                  var sum_d2 = 0.0
                  var in_r = 1
                  var in_c = 1
                  var in_d1 = 1
                  var in_d2 = 1
                  for (i <- 1 to dim) {
                      if (l) {
                          if (xy._1 - i >= 0) {
                              val pl = map(xyToIndex(xy._1 - i, xy._2))
                              if (pl == player) {
                                  sum_r += 1.1
                                  in_r += 1
                              } else if (pl == 0) {
                                  map.get(xyToIndex(xy._1 - i, xy._2 + 1)) match {
                                      case Some(below) if below != 0 =>
                                          in_r += 1
                                      case None =>
                                          in_r += 1
                                      case _ =>
                                          l = false
                                  }
                              } else {
                                  l = false
                              }
                          } else {
                              l = false
                          }
                      }
                      if (r) {
                          if (xy._1 + i < w) {
                              val pl = map(xyToIndex(xy._1 + i, xy._2))
                              if (pl == player) {
                                  in_r += 1
                                  sum_r += 1.1
                              } else if (pl == 0) {
                                  map.get(xyToIndex(xy._1 + i, xy._2 + 1)) match {
                                      case Some(below) if below != 0 =>
                                          in_r += 1
                                      case None =>
                                          in_r += 1
                                      case _ =>
                                          r = false
                                  }
                              } else {
                                  r = false
                              }
                          } else {
                              r = false
                          }
                      }
                      if (d) {
                          val xy2 = xyToIndex(xy._1, xy._2 + i)
                          if (xy2 < w * h) {
                              val pl = list.get(xy2)
                              if (pl == player) {
                                  in_c += 1
                                  sum_c += 1
                              } else if (pl == 0) {
                                  in_c += 1
                              } else {
                                  d = false
                              }
                          } else {
                              d = false
                          }
                      }
                      if (u) {
                          val xy2 = xyToIndex(xy._1, xy._2 - i)
                          if (xy2 >= 0) {
                              val pl = list.get(xy2)
                              if (pl == 0) {
                                  in_c += 1
                              } else {
                                  u = false
                              }
                          } else {
                              u = false
                          }
                      }
                      /*
                      if (b) {
                          if (xy._1 - i >= 0 && xy._2 - i >= 0) {
                              val pl = list.get(xyToIndex(xy._1 - i, xy._2 - i))
                              if (pl == lastPlayer) {
                                  in_d1 += 1
                                  sum_d1 += 0.5
                              } else if (pl == 0) {
                                  in_d1 += 1
                              } else {
                                  b = false
                              }
                          } else {
                              b = false
                          }
                      }
                      if (f) {
                          if (xy._1 + i < w && xy._2 + i < h) {
                              val pl = list.get(xyToIndex(xy._1 + i, xy._2 + i))
                              if (pl == lastPlayer) {
                                  in_d1 += 1
                                  sum_d1 += 0.5
                              } else if (pl == 0) {
                                  in_d1 += 1
                              } else {
                                  f = false
                              }
                          } else {
                              f = false
                          }
                      }
                      if (bb) {
                          if (xy._1 - i >= 0 && xy._2 + i < h) {
                              val pl = list.get(xyToIndex(xy._1 - i, xy._2 + i))
                              if (pl == lastPlayer) {
                                  in_d2 += 1
                                  sum_d2 += 0.5
                              } else if (pl == 0) {
                                  in_d2 += 1
                              } else {
                                  bb = false
                              }
                          } else {
                              bb = false
                          }
                      }
                      if (ff) {
                          if (xy._1 + i < w && xy._2 - i >= 0) {
                              val pl = list.get(xyToIndex(xy._1 + i, xy._2 - i))
                              if (pl == lastPlayer) {
                                  in_d2 += 1
                                  sum_d2 += 0.5
                              } else if (pl == 0) {
                                  in_d2 += 1
                              } else {
                                  ff = false
                              }
                          } else {
                              ff = false
                          }
                      }
                      */
                  }

                  if (in_r < dim) {
                      sum_r = 0
                  }

                  if (in_c < dim) {
                      sum_c = 0
                  } else if (sum_c == 0.0) {
                      sum_c = 0.5
                  }

                  if (in_d1 < dim) {
                      sum_d1 = 0
                  }

                  if (in_d2 < dim) {
                      sum_d2 = 0
                  }

                  var sum = sum_r + sum_c + sum_d1 + sum_d2
                  if (sum_r >= 4 || sum_c >= 4) {
                      sum += 100
                  }
                  sum
              } else {
                  0
              }
          }

        _cachedScore = Option(scores)

        scores
    }

    def place(row: Int, player: Int): Boolean = {
        if (list.get(row) != 0) {
            lastMoveIndex = None
            false
        } else {
            var at = row
            // while spot below at is empty, move down
            while (at + w < w * h && list.get(at + w) == 0) {
                at += w
            }

            list.set(at, player)
            lastPlayer = player

            lastMoveIndex = Option(at)

            _cachedScore = None
            _cachedNext = None

            true
        }
    }

    def show(): Unit = {
        val buf = mutable.ListBuffer.empty[Char]
        list.forEach { v =>
            if (v == 0) {
                buf += ' '
            } else if(v == 1) {
                buf += 'o'
            } else if (v == 2) {
                buf += 'x'
            }
        }
        val hf = List.fill(13)('-').mkString("")
        println(s"|$hf|" + {if (lastPlayer == 1) "x" else "o"})
        buf.sliding(7, 7)
            .foreach(line => println(s"|${line.mkString("|")}|"))
        println(s"|$hf|")
    }

    private var _cachedNext = Option.empty[List[ConnectBoard]]
    def isNextCached: Boolean = _cachedNext.isDefined

    override def next: List[ConnectBoard] = {
        if (_cachedNext.isDefined) {
            return _cachedNext.get
        }

        val moves = validPlaces(nextPlayer).zipWithIndex.filter (_._1 != 0)
        val boards = moves.map { case (_, index) =>
            val board = new ConnectBoard(w, h, dim)
            var i = 0
            list.forEach { v =>
                board.list.set(i, v)
                i += 1
            }

            board.place(index % w, nextPlayer)
            board
        }

        _cachedNext = Some(boards)

        boards
    }

    def nextWithAction: List[(Int, ConnectBoard)] = {
        val moves = validPlaces(nextPlayer).zipWithIndex.filter (_._1 != 0)
        val boards = moves.map { case (_, index) =>
            val board = this.copy()

            board.place(index % w, nextPlayer)
            (index % w, board)
        }

        boards
    }

    def copy(): ConnectBoard = {
        val c = new ConnectBoard(w, h, dim)
        var i = 0
        list forEach { v =>
            c.list.set(i, v)
            i += 1
        }

        c._cachedScore = this._cachedScore
        c._cachedNext = this._cachedNext

        c
    }
}

object Node {
    var NUM_GENERATED = 0
}
class Node[A <: State](val state: A, val parent: Option[Node[A]])(implicit ct: ClassTag[A]) {
    Node.NUM_GENERATED += 1
    lazy val children = state.next.map(c => new Node(c.asInstanceOf[A], Option(this)))
}
