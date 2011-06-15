package sudoku

case class IllegalAssignmentException(val board: Board,
                                      val row: Int,
                                      val col: Int,
                                      val value: Int)
extends RuntimeException {

    override def getMessage() = {
        "(%d,%d)=%d is not valid".format(row, col, value)
    }

}
