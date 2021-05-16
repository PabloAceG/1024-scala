package board

import matrix.Matrix

abstract class AbstractBoard[T <: AbstractBoard[T]] protected (val col: Int,
                                                               val row: Int,
                                                               val points: Int,
                                                               matrix: Matrix[Int]) {

  def moveUp: T
  def moveDown: T
  def moveLeft: T
  def moveRight: T

  def getPrintableBoard: String
}
