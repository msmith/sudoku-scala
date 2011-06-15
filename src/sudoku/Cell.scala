package sudoku;

import collection.immutable.BitSet

class Cell(val row: Int, val col: Int, val possibles: Set[Int]) {

    def this(row: Int, col: Int, value:Int) = this(row, col, Set(value))

    def this(row: Int, col: Int) = this(row, col, Cell.ALL_VALUES)

    val value = if (possibles.size == 1) Some(possibles.head) else None
    
    /**
     * Eliminate a value from the list of possibilities
     */
    def -(v: Int) = new Cell(row, col, possibles - v)

    val isSolved = value.isDefined

    val numPossible = possibles.size

    def isPossible(v: Int) = possibles.contains(v)

    val region = (row / Board.DIM) * Board.DIM + (col / Board.DIM)

    def sameScopeAs(c:Cell) = c.row == row || c.col == col || c.region == region
    
    override def toString = {
        val s = if (isSolved) ("=" + value.get) else (possibles.mkString)
        "(" + row + "," + col + ") " + s 
    }

}

object Cell {
	val ALL_VALUES = BitSet.empty ++ (1 to Board.DIM2)
}
