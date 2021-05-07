package board

import matrix.Matrix

class Board private (val col: Int, val row: Int, matrix: Matrix[Int]) {
  protected def populateBoard(): Board = ???

}


object Board {
  val MINSIZE = 6

  def apply(): Option[JewelsBoard] = apply(MINSIZE, MINSIZE)

  def apply(size: Int): Option[JewelsBoard] = apply(size, size)

  def apply(col: Int, row: Int): Option[JewelsBoard] =
    if (col >= MINSIZE && row >= MINSIZE)
      Some(new Board(col, row, Matrix[Int](col, row).get).populateBoard)
    else None
}
