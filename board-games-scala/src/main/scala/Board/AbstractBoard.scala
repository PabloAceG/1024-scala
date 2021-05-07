package board

import matrix.Matrix

abstract class AbstractBoard private (val col: Int,
                                      val row: Int,
                                      val points: Int,
                                      matrix: Matrix[Int]) {
  def populateBoard: AbstractBoard

  def moveUp: AbstractBoard
  def moveDown: AbstractBoard
  def moveLeft: AbstractBoard
  def moveRight: AbstractBoard

  def getPrintableBoard: String
}
