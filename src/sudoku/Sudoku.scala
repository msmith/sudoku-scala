package sudoku

import scala.io.Source

object Sudoku {

    def main(args: Array[String]): Unit = {
        val s = Source.fromFile(args(0)).mkString
        try {
            val b = Board.read(s)
            println(b)
            println
            println(b.solve.get)
        } catch {
            case e:IllegalAssignmentException => {
                println(e.board)
                println
                println(e.getMessage)
            }
        }
    }
}
