package board

import scala.util.Random

import matrix.Matrix

class JewelsBoard private (override val col: Int,
                           override val row: Int,
                           override val points: Int,
                           matrix: Matrix[Int])
    extends AbstractBoard[JewelsBoard](col, row, points, matrix) {

  def moveUp: JewelsBoard = ???
  def moveDown: JewelsBoard = ???
  def moveLeft: JewelsBoard = ???
  def moveRight: JewelsBoard = ???

  private def moveRight(m: List[Int],
                        col: Int,
                        c: Int,
                        acc: Tuple2[Int, Int],
                        points: Int): List[Int] =
    (m, acc, c) match {
      case (Nil, _, _) => Nil
      case (head :: tail, (num, rep), c) if c == col && num < 3 =>
        List.fill(rep)(num) ::: moveRight(tail, col, 1, (head, 0), points)
      case (head :: tail, (num, rep), c) if c == col && num >= 3 =>
        List.fill(rep)(0) ::: moveRight(tail, col, 1, (head, 0), points + num * rep)
      case (head :: tail, (num, rep), c) if head == num =>
        moveRight(tail, col, c + 1, (num, rep + 1), points)
      case (head :: tail, (num, rep), c) if head != num && num < 3 =>
        List.fill(rep)(num) ::: moveRight(tail, col, c + 1, (head, 0), points)
      case (head :: tail, (num, rep), c) if head != num && num >= 3 =>
        List.fill(rep)(0) ::: moveRight(tail, col, c + 1, (head, 0), points + num * rep)


  }

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
    if (matrix.isDefined) apply(col, row, points,
                                matrix.get.populate(_ => Random.nextInt(8) + 1))
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
