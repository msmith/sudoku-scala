package sudoku;

class Board(val cells: List[Cell]) {

    def get(row: Int, col: Int): Cell = cells(Board.DIM2*row + col)

    def set(row: Int, col: Int, v: Int): Board = {
        val cell = get(row, col)
        if (!cell.isPossible(v)) {
            val msg = "("+ row + "," + col + ")=" + v + " is not valid"
            throw new IllegalStateException(msg)
        }
        val newCells = cells.map { c =>
            if (c == cell)
                new Cell(row, col, v)
            else
            	if (c.sameScopeAs(cell))
            		(c-v)
            	else
            		c
        }
        return new Board(newCells.toList)
    }

    def relatedCells(cell:Cell) = cells.filter(c => c != cell && c.sameScopeAs(cell))

    val isSolved = cells.forall(_.isSolved)

    val unsolvedCells = cells.filter(!_.isSolved)

    def solve:Option[Board] = {
        if (isSolved)
            return Some(this)
        val c = unsolvedCells.sort(_.numPossible < _.numPossible).first
        for (v <- c.possibles) {
            val b = set(c.row, c.col, v)
            val s = b.solve
            if (s.isDefined) {
                return s
            }
        }
        return None
    }
    
    override def toString = cells.foldLeft("") { (s, c) =>
        val s1 = s + c.value.getOrElse('.')
        if (c.isLastInCol) (s1 + "\n") else (s1)
    }

}

object Board {
    val DIM = 3
    val DIM2 = DIM*DIM
    val MAX_IDX = DIM2 - 1

    val blank: Board = {
        var cells:List[Cell] = Nil
        for (row <- 0.to(MAX_IDX); col <- 0.to(MAX_IDX))
        	cells ++= List(new Cell(row, col))
        new Board(cells)
    }

    /* Reads a puzzle in multi-line format
     */
    def read(s:String): Board = {
        var b = Board.blank
        var row = 0
        for (line <- s.split("\n")) {
            val digits = line.map(Character.digit(_,10))
            var col = 0
            for (d <- digits) {
                if (d > 0)
                	b = b.set(row, col, d)
                col += 1
            }
            row += 1
        }
        return b
    }
    
    /* Reads a puzzle in single-line format
     */
    def read2(s:String): Board = {
        var b = Board.blank
        var row = 0
        var col = 0
        for (c <- s) {
            if (c != '0') {
            	val d = Character.digit(c,10)
                b = b.set(row, col, d)
            }
            col += 1
            if (col > MAX_IDX) {
                col = 0
                row += 1
            }
        }
        return b
    }

}