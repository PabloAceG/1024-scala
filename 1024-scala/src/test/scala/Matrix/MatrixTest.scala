package matrix

import org.scalatest.funsuite.AnyFunSuite

class MatrixTest extends AnyFunSuite {

  test ("should not be possible to create an empty matrix") {
    val m = Matrix[Int](0, 0)
    assert(!m.isDefined)
  }

  test ("convert matrix to string") {
    val m = Matrix[Int](3, 3).get
    val mInStr = m.toString
    val correctStr = "[ [ 0 0 0 ] [ 0 0 0 ] [ 0 0 0 ] ]"
    assert(mInStr == correctStr)
  }

  test ("transpose square matrix") {
    val m = Matrix[Int](List(1, 2, 3, 4, 5, 6, 7, 8, 9)).get
    val mInStr = m.transposeMatrix.toString
    val correctStr = "[ [ 1 4 7 ] [ 2 5 8 ] [ 3 6 9 ] ]"
    assert(mInStr == correctStr)
  }

  test ("transpose rectangular matrix") {
    val m = Matrix[Int](4, 3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)).get
    val mInStr = m.transposeMatrix.toString()
    val correctStr = "[ [ 1 5 9 ] [ 2 6 10 ] [ 3 7 11 ] [ 4 8 12 ] ]"
    assert(mInStr == correctStr)
  }
}
