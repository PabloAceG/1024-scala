package matrix

import scala.collection.Iterable
import scala.collection.Iterator
import scala.collection.AbstractIterator
import scala.math.sqrt
import scala.math.floor

import defaultValues.Default

class Matrix[A: Default] private (val col: Int, val row: Int, elems: List[A])
    extends Iterable[A] { self =>

  def apply(i: Int): A = elems(i).asInstanceOf[A]

  def iterator: Iterator[A] = new AbstractIterator[A] {
    private var current = 0
    def hasNext = current < elems.size
    def next(): A = {
      val elem = self(current)
      current += 1
      elem
    }
  }

  def transposeMatrix(): Matrix[A] = transpose(elems, List(), row, col, 0, 0)

  private def transpose(m: List[A],
                        newM: List[A],
                        col: Int,
                        row: Int,
                        c: Int,
                        r: Int): Matrix[A] =
    if (r == row) Matrix[A](col, row, newM).get
    else (c, r) match {
      case (0, 0) => transpose(m, (m(0) :: Nil), col, row, 1, 0)
      case (c, r) if c == col => transpose(m, newM, col, row, 0, r + 1)
      case (c, r) => transpose(m, newM :+ m(row * c + r), col, row, c + 1, r)
  }

  @`inline` def == (m2: Matrix[A]): Boolean = compareMatrices(self, m2)
  @`inline` def == (m2: List[A]): Boolean = compareMatrices(elems, m2)

  @`inline` def != (m2: Matrix[A]): Boolean = !compareMatrices(self, m2)
  @`inline` def != (m2: List[A]): Boolean = !compareMatrices(elems, m2)

  private def compareMatrices[A](m1: Matrix[A], m2: Matrix[A]): Boolean =
    if (m1.dimensions == m2.dimensions) compareMatrices(m1.toList, m2.toList)
    else false

  private def compareMatrices[A](m1: List[A], m2: List[A]): Boolean =
    (m1, m2) match {
      case (Nil, Nil) => true
      case (h1 :: t1, h2 :: t2) if (h1 == h2) => compareMatrices(t1, t2)
      case _ => false
  }

  def populate[B](f: (A) => B): Matrix[B] = Matrix[B](col, row, elems.map(f)).get

  def dimensions: (Int, Int) = (col, row)

  override def size: Int = col * row

  override def toList: List[A] = self.elems

  override def toString: String = matrixToStr(elems, col, row, col, row)

  private def matrixToStr(m: List[A],
                          col: Int,
                          row: Int,
                          c: Int,
                          r: Int): String =
    (m, c, r) match {
      case (Nil, _, _) => "] ]"
      case (m, c, r) if c == col && r == row =>
        "[ " + matrixToStr(m, col, row, c, r - 1)
      case (m, c, r) if c == col => "[ " + matrixToStr(m, col, row, c - 1, r)
      case (m, -1, r) => "] " + matrixToStr(m, col, row, col, r - 1)
      case (head :: tail, c, r) =>
        head.toString + " " + matrixToStr(tail, col, row, c - 1, r)
  }

  override def className = "Matrix"

}

object Matrix {
  private val MINSIZE = 1
  private val DEFAULTSIZE = 4

  def apply[A: Default](): Option[Matrix[A]] = apply[A](DEFAULTSIZE, DEFAULTSIZE)

  def apply[A: Default](size: Int): Option[Matrix[A]] = apply[A](size, size)

  def apply[A: Default](col: Int, row: Int): Option[Matrix[A]] =
    apply[A](col, row, List.fill(col * row)(Default.value[A]))

  def apply[A: Default](elems: List[A]): Option[Matrix[A]] = {
    val length = elems.size
    val size = sqrt(length)
    if (size == floor(size)) apply[A](size.toInt, size.toInt, elems)
    else None
  }

  def apply[A: Default](col: Int, row: Int, elems: List[A]): Option[Matrix[A]] =
    if (col >= MINSIZE && row >= MINSIZE && elems.size == col * row)
      Some(new Matrix[A](col, row, elems))
    else None

}
