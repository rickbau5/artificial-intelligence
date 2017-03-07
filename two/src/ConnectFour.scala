import scala.collection.mutable
import scala.io.StdIn
import scala.util.{Random, Try}
import collection.JavaConverters._
import scala.reflect.ClassTag

/**
  * Created by Rick on 2/22/2017.
  */
object ConnectFour {

    val scoreR = raw"s ([12])".r

    def main(args: Array[String]): Unit = {
        val board = new ConnectBoard(7, 6)
        board.show()

        var player_b = true
        def player = if (player_b) 1 else 2
        while (true) {
            StdIn.readLine("> ") match {
                case n if Try(n.toInt).toOption.exists(i => i >= 0 && i < 7) =>
                val placed = board.place(n.toInt, player)
                if (!placed) {
                    println("Column full, try again.")
                } else {
                    player_b = !player_b
                    val (victor, _) = board.victor
                    if (victor != 0) {
                        println(s"Player $victor wins!")
                        board.show()
                        sys.exit()
                    }
                }

                board.show()

                case "a" =>
                    def choose(pl: Int = 2, _print: Boolean = false): (Double, Int) = {
                        val scores = board.score(pl).zipWithIndex
                        val max = scores.maxBy(_._1)
                        val maxes = scores.filter(_._1 == max._1)

                        if (_print) {
                            scores.sliding(7, 7)
                              .foreach(r => println(r.map(_._1).map(e => "%1.1f".format(e)).mkString(" ")))
                        }
                        maxes(Random.nextInt(maxes.length))
                    }

                    val choiceActive = choose(pl = player, _print = true)
                    val choiceOther = choose(pl = if (player == 1) 2 else 1)
                    println(choiceOther._2 % board.w)

                    val choice = if (choiceActive._1 < choiceOther._1) {
                        println("Player has a higher score, going to disrupt.")
                        board.score(if (player == 1) 2 else 1)
                          .sliding(7, 7)
                          .foreach(row => println(row.map(v => "%1.1f".format(v)).mkString(" ")))

                        choiceOther
                    } else {
                        choiceActive
                    }

                    board.place(choice._2 % board.w, player)
                    player_b = !player_b

                    val victor = board.victor
                    if (victor._1 != 0) {
                        println(s"Player $victor wins!")
                        board.show()
                        sys.exit()
                    } else {
                        board.show()
                    }

                case "s" =>
                    val scores = board.score()
                    println("Here's the score for the current board for " + (if (player_b) "o" else "x"))
                    scores.sliding(7, 7)
                      .foreach(row => println(row.map(e => "%1.1f".format(e)).mkString(" ")))

                case "mm" =>
                    println(s"Minmax for $player")
                    minmax(board, player)

                case "play" =>
                    println("HI! I'm Al, I'm going to play with myself ;)")
                    var victor = 0
                    while (victor == 0) {
                        def choose(pl: Int = 2, _print: Boolean = false): (Double, Int) = {
                            val scores = board.score(pl).zipWithIndex
                            val max = scores.maxBy(_._1)
                            val maxes = scores.filter(_._1 == max._1)

                            if (_print) {
                                scores.sliding(7, 7)
                                  .foreach(r => println(r.map(_._1).map(e => "%1.1f".format(e)).mkString(" ")))
                            }
                            maxes(Random.nextInt(maxes.length))
                        }

                        val choiceActive = choose(pl = player, _print = true)
                        val choiceOther = choose(pl = if (player == 1) 2 else 1)
                        println(choiceOther._2 % board.w)

                        val choice = if (choiceActive._1 < choiceOther._1) {
                            println("Player has a higher score, going to disrupt.")
                            board.score(if (player == 1) 2 else 1)
                              .sliding(7, 7)
                              .foreach(row => println(row.map(v => "%1.1f".format(v)).mkString(" ")))

                            choiceOther
                        } else {
                            choiceActive
                        }

                        board.place(choice._2 % board.w, player)
                        player_b = !player_b

                        victor = board.victor._1
                        if (victor != 0) {
                            println(s"Player $victor wins!")
                            board.show()
                            sys.exit()
                        } else {
                            board.show()
                        }

                        Thread.sleep(1700)
                    }

                case scoreR(strPl) =>
                    val pl = strPl.toInt
                    val scores = board.score(pl)
                    println("Here's the score for the current board for " + (if (pl == 1) "o" else "x"))
                    scores.sliding(7, 7)
                      .foreach(row => println(row.map(e => "%1.1f".format(e)).mkString(" ")))

                case _ =>
                    println("Invalid command.")
            }
        }
    }

    def utility(board: ConnectBoard, player: Int): Int = board.victor match {
        case (1, _) if player == 1 =>
            1
        case (2, _) if player == 2 =>
            -1
        case _ =>
            0
    }

    def minmax(start: ConnectBoard, player: Int): Unit = {
        def max(node: Node[ConnectBoard])(depth: Int): Double = {
            val next = node.state.next
            if (depth == 0 || next.isEmpty || node.state.victor._1 != 0) {
                utility(node.state, player)
            } else {
                var v = Double.MinValue
                next foreach { child =>
                    val res = min(new Node(child, Option(node)))(depth - 1)
                    if (res > v) {
                        v = res
                    }
                }
                v
            }
        }

        def min(node: Node[ConnectBoard])(depth: Int): Double = {
            val next = node.state.next
            if (depth == 0 || next.isEmpty || node.state.victor._1 != 0) {
                utility(node.state, 1 + player % 2)
            } else {
                var v = Double.MaxValue
                next foreach { child =>
                    val res = max(new Node(child, Option(node)))(depth - 1)
                    if (res < v) {
                        v = res
                    }
                }
                v
            }
        }

        val ret = max(new Node(start, None))(3)
        println(ret)
    }
}

trait State {
    def next: List[State]
}
class ConnectBoard(val w: Int, val h: Int, val dim: Int = 4) extends State {
    val list = new java.util.ArrayList[Integer](w * h)
    (0 until w * h).foreach(_ => list.add(0))

    var lastMoveIndex = Option.empty[Int]
    var lastPlayer = 2

    def indexToXY(idx: Int): (Int, Int) = (idx % w, idx / w)
    def xyToIndex(xy: (Int, Int)): Int = xy._2 * w + xy._1

    /**
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
                println("Row")
                (lastPlayer, last)
            } else if (sum_c >= 4) {
                println("Column")
                (lastPlayer, last)
            } else if (sum_d1 >= 4 || sum_d2 >= 4) {
                println("Diagonal")
                (lastPlayer, last)
            } else {
                (0, last)
            }
        case None =>
            (0, 0)
    }

    def validPlaces(playerToScore: Int): List[Int] = {
        val map = list.asScala.zipWithIndex.map(_.swap).toMap
        list.asScala.zipWithIndex
          .foldLeft(List.empty[Int]) { case (ls, (player, index)) =>
              if (player == 0) {
                  ls ++ (map.get(index + w) match {
                      case Some(below) if below != 0 => List(playerToScore)
                      case None => List(playerToScore)
                      case _ => List(0)
                  })
              } else {
                  ls ++ List(0)
              }
          }
    }

    def score(playerToScore: Int = if (lastPlayer == 1) 2 else 1): List[Double] = {
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
                                  /* score based how many empty squares below. Doesn't work because validPlaces
                                  var j = 1
                                  var below = map.get(xyToIndex((xy._1 - i, xy._2 + j)))
                                  while (below.getOrElse(-1) == 0) {
                                      j += 1
                                      below = map.get(xyToIndex(xy._1 - i, xy._2 + j))
                                  }
                                  sum_r += 0.9 / j
                                  in_r += 1
                                  */

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

                  val sum = sum_r + sum_c + sum_d1 + sum_d2
                  sum
              } else {
                  0
              }
          }

        scores

        // Debug
        // show()
        // validPlaces.sliding(7, 7)
        //   .foreach { row => println(row.mkString(" ")) }
        // scores.sliding(7, 7)
        //   .foreach { row => println(row.mkString(" ")) }
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

    override def next: List[ConnectBoard] = {
        val moves = validPlaces(if (lastPlayer == 2) 1 else 2).zipWithIndex.filter (_._1 != 0)
        val boards = moves.map { case (_, index) =>
            val board = new ConnectBoard(w, h, dim)
            var i = 0
            list.forEach { v =>
                board.list.set(i, v)
                i += 1
            }

            board.place(index % w, if (lastPlayer == 2) 1 else 2)
            board
        }

        boards
    }
}

object Node {
    var NUM_GENERATED = 0
}
class Node[A <: State](val state: A, val parent: Option[Node[A]])(implicit ct: ClassTag[A]) {
    lazy val children = state.next
}
