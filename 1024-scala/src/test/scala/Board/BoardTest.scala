package Board

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.PrivateMethodTester

class MatrixTest extends AnyFunSuite with PrivateMethodTester {

  test ("create and populate board with smaller size that permitted") = {
    val b = Board(6, 5)
    assert(!b.isDefined)
  }

  test ("create and populate board using standard constructor") = (pending)

  test ("create and populate board using square board constructor") = (pending)

  test ("create and populate board using rectangular board constructor") = {
    val b = Board()
    assert(b.isDefined)
  }

  test ("populate empty board") (pending)
}
