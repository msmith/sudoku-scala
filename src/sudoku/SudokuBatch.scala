package sudoku

import scala.io.Source

object SudokuBatch {

    def main(args: Array[String]): Unit = {
        val f = args(0)
        
        println("Solving boards from " + f)
        println
        
        var solved = 0
        var total = 0
        
        val lines = Source.fromFile(f).mkString.lines
        
        val t0 = System.currentTimeMillis()
        
        for (s <- lines) {
        	val b = Board.read2(s)
        	if (b.solve.isDefined) {
        		solved +=1
        	}
        	total += 1
         
        	val pct = 100*solved/total
        	val ms = (System.currentTimeMillis() - t0)
        	val rate = 1000.0*total/ms
        	
        	printf("solved %d/%d (%d%%) %2.2f per second\n", solved, total, pct, rate);
        }
    }
  
}
