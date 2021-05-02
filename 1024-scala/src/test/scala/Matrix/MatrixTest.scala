package matrix

import org.scalatest.funsuite.AnyFunSuite

class MatrixTest extends AnyFunSuite {

  test ("should not be possible to create an empty matrix") {
    val m = Matrix[Int](0, 0)
    assert(!m.isDefined)
  }

  test ("transpose matrix") (pending)

  test ("convert matrix to string") {
    val m = Matrix[Int](3, 3).get
    val mInStr = m.toString
    val correctStr = "[ [ 0 0 0 ] [ 0 0 0 ] [ 0 0 0 ] ]"
    assert(mInStr == correctStr)
  }

  test ("get element at position") (pending)

  test ("get element at negative position") (pending)
}
