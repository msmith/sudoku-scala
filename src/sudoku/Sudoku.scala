package sudoku

import scala.io.Source

object Sudoku {

    def main(args: Array[String]): Unit = {
        val s = Source.fromFile(args(0)).mkString
        val b = Board.read(s)
        println(b.solve.get)
    }
}
