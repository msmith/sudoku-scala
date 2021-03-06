package sudoku

import scala.io.Source

object SudokuBatch {

    def main(args: Array[String]): Unit = {
        val source = if (args.length > 0) Source.fromFile(args(0)) else Source.fromInputStream(System.in)

        var solved = 0
        var total = 0

        val lines = source.getLines

        val t0 = System.currentTimeMillis()

        try {
            for (s <- lines) {
                val b = Board.read(s)
                if (b.solve.isDefined) {
                    solved +=1
                }
                total += 1

                val pct = 100*solved/total
                val ms = (System.currentTimeMillis() - t0)
                val rate = 1000.0*total/ms

                printf("solved %d/%d (%d%%) %2.2f per second\n", solved, total, pct, rate);
            }
        } catch {
            case e:IllegalAssignmentException => {
                println(e.board)
                println
                println(e.getMessage)
            }
        }
    }
  
}
