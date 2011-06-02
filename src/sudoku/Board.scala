package sudoku;

class Board(val cells: List[Cell]) {

    def get(row: Int, col: Int): Cell = cells(Board.DIM2*row + col)

    def set(row: Int, col: Int, v: Int): Board = {
        val cell = get(row, col)
        if (!cell.isPossible(v)) {
            val msg = "("+ row + "," + col + ")=" + v + " is not valid"
            throw new IllegalArgumentException(msg)
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

    val unsolvedCells = cells.filterNot(_.isSolved)

    def solve:Option[Board] = {
        if (isSolved)
            return Some(this)
        val c = unsolvedCells.minBy(_.numPossible)
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
        val x = c.value.getOrElse('.')
        val hSpace = c.row % Board.DIM == (Board.DIM-1)
        val vSpace = c.col % Board.DIM == (Board.DIM-1)
        val lastInCol = c.col == Board.MAX_IDX
        s + x + ((hSpace, vSpace, lastInCol) match {
          case (_   ,true,false) => "  "
          case (true,_   ,true)  => "\n\n"
          case (_   ,_   ,true)  => "\n"
          case _                 => " "
        })
    }

}

object Board {
    val DIM = 3
    val DIM2 = DIM*DIM
    val MAX_IDX = DIM2 - 1

    private val SZ = DIM2*DIM2

    val COORDS = 0.to(SZ-1).map( i => (i / DIM2, i % DIM2)).toList

    val blank: Board = {
        val idxs = 0.to(MAX_IDX)
        val cells = for (row <- idxs; col <- idxs) yield new Cell(row, col)
        new Board(cells.toList)
    }

    def read(s:String): Board = {
        var b = Board.blank
        val vals = toVals(s)
        for ( ((row, col),v) <- COORDS.zip(vals) ) {
            if (v.isDefined) {
                b = b.set(row, col, v.get)
            }
        }
        return b
    }
    
    private def toVals(s:String):List[Option[Int]] = {
      val s1 = s.filter(_ != '\n')
      if (s1.length != SZ) {
        throw new IllegalArgumentException("puzzle must be " + SZ + " cells")
      }
      s1.map { c =>
        val d = Character.digit(c,10)
        if (d > 0) Some(d) else None
      }.toList
    }

}
