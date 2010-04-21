package sudoku

import scala.io.Source
import java.io.File

object Sudoku {

    def main(args: Array[String]): Unit = {
        val s = Source.fromFile(new File(args(0))).mkString
        val b = Board.read(s)
        println(b)
        println
        println(b.solve.get)
    }
}
