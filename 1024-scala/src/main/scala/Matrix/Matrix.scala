package matrix

import scala.collection.Iterable
import scala.collection.Iterator
import scala.collection.AbstractIterator

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

  override def className = "Matrix"

  override def toList: List[A] = self.elems

  override def toString: String = matrixToStr(elems, col, row, col, row)

  private def matrixToStr(m: List[A],
                          col: Int,
                          row: Int,
                          c: Int,
                          r: Int): String = {
    (m, c, r) match {
      case (Nil, _, _) => "] ]"
      case (m, c, r) if c == col && r == row =>
        "[ " + matrixToStr(m, col, row, c, r - 1)
      case (m, c, r) if c == col => "[ " + matrixToStr(m, col, row, c - 1, r)
      case (m, -1, r) => "] " + matrixToStr(m, col, row, col, r - 1)
      case (head :: tail, c, r) =>
        head.toString + " " + matrixToStr(tail, col, row, c - 1, r)
    }
  }
}

object Matrix {
  def apply[A: Default](): Option[Matrix[A]] = apply[A](4, 4)

  def apply[A: Default](size: Int): Option[Matrix[A]] = apply[A](size, size)

  def apply[A: Default](col: Int, row: Int): Option[Matrix[A]] =
    if (col > 0 || row > 0)
      Some(new Matrix[A](col, row, List.fill(col * row)(Default.value[A])))
    else None
}
