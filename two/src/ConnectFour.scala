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
                case _ =>
                ;
            }
        }
    }
}

class ConnectBoard(w: Int, h: Int, dim: Int = 4) {
    val list = new java.util.ArrayList[Integer](w * h)
    (0 until w * h).foreach(_ => list.add(0))

    var lastPlayer = 2

    def checkRows(list: List[Integer]): Int =  {
        list.sliding(w, w).foldLeft(0) { case (winner, row) =>
            if (winner != 0) {
                winner
            } else {
                val (p, c) = row.foldLeft((0, 0)) { case ((player, consecutive), v) =>
                    if (consecutive == dim) {
                        (player, dim)
                    } else {
                        if (v == player) {
                            (player, consecutive + 1)
                        } else {
                            (v, 1)
                        }
                    }
                }
                if (c == dim) {
                    p
                } else {
                    0
                }
            }
        }
    }

    def checkDiagonal(map: Map[(Int, Int), Integer], inc: Int): Int = {
        map.foldLeft(0) { case (winner, ((x, y), player)) =>
            if (winner != 0) {
                winner
            } else {
                if (player != 0) {
                    val ret = (1 until dim).foldLeft(1) { case (cons, i) =>
                        if (x + inc < h && y + inc < w && x + inc > 0 && y + inc > 0) {
                            if (map(x + inc, y + inc) == player) {
                                cons + 1
                            } else {
                                0
                            }
                        } else {
                            0
                        }
                    }
                    if (ret == dim) {
                        player.toInt
                    } else {
                        0
                    }
                } else {
                    winner
                }
            }
        }
    }

    def victor: Int = {
        val scalaList = list.asScala.toList
        val rw = checkRows(scalaList)

        if (rw == 0) {
            val transpose = (0 until w).flatMap { i =>
                (0 until h).map(j => list.get(i + j * w)).toList
            }.toList

            val cw = checkRows(transpose)

            if (cw == 0) {
                val map = scalaList.zipWithIndex.map { case (v, i) => ((i / w, i % w), v) }
                  .toMap

                val dw = checkDiagonal(map, 1)
                if (dw == 0) {
                    val drw = checkDiagonal(map, -1)
                    if (drw != 0)
                        println("Reverse Diagonal")
                    drw
                } else {
                    println("Diagonal")
                    dw
                }
            } else {
                println("Column")
                cw
            }
        } else {
            println("Row")
            rw
        }
    }

    def victor_old: Int = {
        var w = 0
        var n = 0
        var l = list.get(0)
        list.forEach { i: Integer =>
            i.toInt match {
                case _ if w != 0 => ;
                case 0 => ;
                case 1 =>
                    if (l == 1) {
                        n += 1
                        if (n == dim) {
                            w = 1
                        }
                    } else {
                        n = 1
                        l = 1
                    }
                case 2 =>
                    if (l == 2) {
                        n += 1
                        if (n == dim) {
                            w = 2
                        }
                    } else {
                        n = 1
                        l = 2
                    }
            }
        }

        w
    }

    def place(row: Int, player: Int): Boolean = {
        if (list.get(row) != 0) {
            false
        } else {
            var at = row
            // while spot below at is empty, move down
            while (at + w < w * h && list.get(at + w) == 0) {
                at += w
            }
            list.set(at, player)
            lastPlayer = player

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
