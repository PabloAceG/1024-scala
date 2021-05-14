package board

import scala.util.Random

import matrix.Matrix

class JewelsBoard private (override val col: Int,
                           override val row: Int,
                           override val points: Int,
                           matrix: Matrix[Int])
    extends AbstractBoard[JewelsBoard](col, row, points, matrix) {


  override def populateBoard: JewelsBoard =
    JewelsBoard(col, row, points, populate(matrix.toList)).get

  private def populate(board: List[Int]): List[Int] =
    board match {
      case Nil => Nil
      case head :: tail =>
        Random.nextInt(JewelsBoard.NUMJEWELS) :: populate(tail)
    }

  def moveUp: JewelsBoard = ???
  def moveDown: JewelsBoard = ???
  def moveLeft: JewelsBoard = ???
  def moveRight: JewelsBoard = ???

  def getPrintableBoard: String = ???
}

object JewelsBoard {
  val NUMJEWELS = 8
  val MINSIZE = 6

  def apply(): Option[JewelsBoard] = apply(MINSIZE, MINSIZE, 0)

  def apply(size: Int): Option[JewelsBoard] = apply(size, size, 0)

  def apply(col: Int, row: Int): Option[JewelsBoard] = apply(col, row, 0)

  def apply(col: Int, row: Int, points: Int): Option[JewelsBoard] = {
    val matrix = Matrix[Int](col, row)
    if (matrix.isDefined) apply(col, row, points, matrix.get)
    else None
  }

  def apply(col: Int, row: Int, points: Int, m: List[Int]): Option[JewelsBoard] = {
    val matrix = Matrix[Int](col, row, m)
    if (matrix.isDefined) apply(col, row, points, matrix.get)
    else None
  }

  def apply(col: Int, row: Int, points: Int, m: Matrix[Int]): Option[JewelsBoard] =
    if (col >= MINSIZE && row >= MINSIZE && m.dimensions == (col, row))
      Some(new JewelsBoard(col, row, points, m))
    else None
}
