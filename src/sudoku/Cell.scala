package sudoku

import collection.immutable.BitSet

class Cell(val row: Int, val col: Int, val possibles: Set[Int]) {

    def this(row: Int, col: Int, value:Int) = this(row, col, Set(value))

    def this(row: Int, col: Int) = this(row, col, Cell.ALL_VALUES)

    val region = (row / Board.DIM) * Board.DIM + (col / Board.DIM)

    val value = if (numPossible == 1) Some(possibles.head) else None

    val isSolved = value.isDefined

    val numPossible = possibles.size

    /**
     * Eliminate a value from the list of possibilities
     */
    def -(v: Int) = new Cell(row, col, possibles - v)

    def isPossible(v: Int) = possibles.contains(v)

    def sameScopeAs(c:Cell) = c.row == row || c.col == col || c.region == region
    
    override def toString = {
        "(%d,%d) %s".format(row, col, possibles.mkString)
    }

}

object Cell {
    val ALL_VALUES = BitSet.empty ++ (1 to Board.DIM2)
}
