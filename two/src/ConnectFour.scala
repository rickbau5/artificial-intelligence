import scala.collection.mutable
import scala.io.StdIn
import scala.util.Try
import collection.JavaConverters._

/**
  * Created by Rick on 2/22/2017.
  */
object ConnectFour {
    def main(args: Array[String]): Unit = {
        val board = new ConnectBoard(7, 6)
        board.show()

        var player = true
        while (true) {
            StdIn.readLine("> ") match {
                case n if Try(n.toInt).toOption.exists(i => i >= 0 && i < 7) =>
                val placed = board.place(n.toInt, if (player) 1 else 2)
                if (!placed) {
                    println("Invalid move.")
                } else {
                    player = !player
                    val victor = board.victor
                    if (victor != 0) {
                        println(s"Player $victor wins!")
                        board.show()
                        sys.exit()
                    }
                }

                board.show()

                case "h" =>
                    heuristic(board)
                case _ =>
                ;
            }
        }
    }

    def emptyNeighbors(xy: (Int, Int), map: Map[(Int, Int), Int]): List[(Int, Int)] = List(
            (xy._1 - 1, xy._2) -> map.get(xy._1 - 1, xy._2),
            (xy._1 + 1, xy._2) -> map.get(xy._1 + 1, xy._2),
            (xy._1, xy._2 - 1) -> map.get(xy._1, xy._2 - 1),
            (xy._1, xy._2 + 1) -> map.get(xy._1, xy._2 + 1),
            (xy._1 - 1, xy._2 - 1) -> map.get(xy._1 - 1, xy._2 - 1),
            (xy._1 + 1, xy._2 + 1) -> map.get(xy._1 + 1, xy._2 + 1),
            (xy._1 - 1, xy._2 + 1) -> map.get(xy._1 - 1, xy._2 + 1),
            (xy._1 + 1, xy._2 - 1) -> map.get(xy._1 + 1, xy._2 - 1)
        ).foldLeft(List.empty[(Int, Int)]) { case (list, ((x, y), opt)) =>
            opt match {
                case Some(0) =>
                    if (map.get(x, y + 1).getOrElse(3) != 0)
                        list ++ List((x, y))
                    else
                        list
                case _ =>
                    list
            }
        }

    def heuristic(board: ConnectBoard): Int = {
        val scores = mutable.Map((0 until board.w * board.h).map(i => board.indexToXY(i) -> 0):_*)
        val map = board.list.asScala.zipWithIndex.map { case (v, idx) =>
            board.indexToXY(idx) -> v.toInt
        }.toMap
        map.foreach { case ((x, y), player) =>
            if (player == board.lastPlayer) {
                emptyNeighbors((x, y), map).foreach(xy => scores(xy) += 1)
            }
        }
        val hf = List.fill(13)('-').mkString("")
        println(s"|$hf|" + {if (board.lastPlayer == 1) "o" else "x"})
        scores.toList.map(e => board.xyToIndex(e._1) -> e._2).sortBy(_._1).sliding(7, 7)
          .foreach(v => println(s"|${v.map(_._2).mkString("|")}|"))
        println(s"|$hf|")

        0
    }
}

class ConnectBoard(val w: Int, val h: Int, val dim: Int = 4) {
    val list = new java.util.ArrayList[Integer](w * h)
    (0 until w * h).foreach(_ => list.add(0))

    var lastMoveIndex = Option.empty[Int]
    var lastPlayer = 2

    def indexToXY(idx: Int): (Int, Int) = (idx % w, idx / w)
    def xyToIndex(xy: (Int, Int)): Int = xy._2 * w + xy._1

    def victor: Int = lastMoveIndex match {
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
            var sum_d = 1

            for (i <- 1 to 3 if sum_r < dim & sum_c < dim & sum_d < dim) {
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
                    if (xy._1 - i >= 0 && xy._2 >= 0) {
                        if (list.get(xyToIndex(xy._1 - i, xy._2 - i)) == lastPlayer) {
                            sum_d += 1
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
                            sum_d += 1
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
                            sum_d += 1
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
                            sum_d += 1
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
                lastPlayer
            } else if (sum_c >= 4) {
                println("Column")
                lastPlayer
            } else if (sum_d >= 4) {
                println("Diagonal")
                lastPlayer
            } else {
                0
            }
        case None =>
            0
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
}
