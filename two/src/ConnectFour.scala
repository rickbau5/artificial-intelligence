import scala.collection.mutable
import scala.io.StdIn
import scala.util.Try

/**
  * Created by Rick on 2/22/2017.
  */
object ConnectFour {

  def main(args: Array[String]): Unit = {
    val board = new Board
    board.show()

    var player = true
    while (true) {
      StdIn.readLine("> ") match {
        case n if Try(n.toInt).toOption.exists(i => i >= 0 && i < 7) =>
          board.place(n.toInt, if (player) 1 else 2)
          player = !player

          board.show()
        case _ =>
          ;
      }
    }
  }
}

class Board {
  val list = new java.util.ArrayList[Integer](42)
  (0 until 6*7).foreach(_ => list.add(0))

  def place(row: Int, player: Int): Board = {
    if (list.get(row) != 0) {
      this
    } else {
      var at = row
      while (at + 7 < 42 && list.get(at + 7) == 0) {
        at += 7
      }
      list.set(at, player)

      this
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
    println(s"|$hf|")
    buf.sliding(7, 7)
      .foreach(line => println(s"|${line.mkString("|")}|"))
    println(s"|$hf|")
  }
}
