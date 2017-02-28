import collection.JavaConverters._

val board = new ConnectBoard(7, 6)

board.place(0, 1)
board.place(0, 1)
board.place(0, 1)
board.place(1, 1)
board.place(3, 1)
board.place(4, 2)

val lastPlayer = 1

val map = board.list.asScala.zipWithIndex.map(_.swap).toMap

val validPlaces = board.list.asScala.zipWithIndex
  .foldLeft(List.empty[Int]) { case (list, (player, index)) =>
      if (player == 0) {
          list ++ (map.get(index + board.w) match {
              case Some(below) if below != 0 => List(lastPlayer)
              case None => List(lastPlayer)
              case _ => List(0)
          })
      } else {
          list ++ List(0)
      }
  }

val scores = validPlaces.zipWithIndex
  .map { case (player, index) =>
      if (player == lastPlayer) {
          val xy = board.indexToXY(index)
          var l = true
          var r = true
          var d = true
          var sum_r = 1
          var sum_c = 1
          var in_r = 1
          var in_c = 1
          for (i <- 1 to board.dim) {
              if (l) {
                  if (xy._1 - i >= 0) {
                      if (map(board.xyToIndex(xy._1 - i, xy._2)) == player) {
                          sum_r += 1
                      } else {
                          l = false
                      }
                  } else {
                      l = false
                  }
              }
              if (r) {
                  if (xy._1 + i < board.w) {
                      if (map(board.xyToIndex(xy._1 + i, xy._2)) == player) {
                          sum_r += 1
                      } else {
                          r = false
                      }
                  } else {
                      r = false
                  }
              }
              if (d) {
                   val xy2 = board.xyToIndex(xy._1, xy._2 + i)
                   if (xy2 < board.w * board.h && board.list.get(xy2) == lastPlayer) {
                       sum_c += 1
                   } else {
                       d = false
                   }
              }
          }
          sum_r + sum_c
      } else {
          0
      }
  }

board.show()
validPlaces.sliding(7, 7)
  .foreach { row => println(row.mkString(" ")) }
scores.sliding(7, 7)
  .foreach { row => println(row.mkString(" ")) }
