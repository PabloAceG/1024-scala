import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.PrivateMethodTester
import javax.management.loading.PrivateMLet

class MatrixTest extends AnyFunSuite
    with PrivateMethodTester {
  test("matrix smaller than 4 cannot be created") {
    val m = Matrix(3)
    assert(!m.isDefined)
  }

  test("matrix with size 4 can be created") {
    val m = Matrix(4)
    assert(m.isDefined)
  }

  test("matix with size 10 can be created") {
    val m = Matrix(10)
    assert(m.isDefined)
  }

  test("matrix with size greater than 10 cannot be created") {
    val m = Matrix(100)
    assert(!m.isDefined)
  }

  test("matrix of size 4 can be initialized") {
    val m = Matrix(4).get.matrix
    val testMatrix = Array(
      Array(0, 0, 0, 0),
      Array(0, 0, 0, 0),
      Array(0, 0, 0, 0),
      Array(0, 0, 0, 0))
    assert(Matrix.compareMatrices(m, testMatrix))
  }

  test("number of seeds relates to matrix's size") {
    val testExpectedSeeds = Vector(2, 2, 3, 3, 4, 4, 5)
    // Private methods cannot usually be accessed outside class. Need this:
    val numberSeedsOnSize = PrivateMethod[Int]('numberSeedsOnSize)
    val seeds = for(i <- Matrix.MINSIZE to Matrix.MAXSIZE)
      yield(Matrix(i).get invokePrivate numberSeedsOnSize())
    assert(seeds == testExpectedSeeds)
  }

  test("select positions to seed") (pending)

  test("find positions with no tile (empty tiles)") (pending)

  test("populate matrix's first column when it's empty") {
    // 0 0 0 0     2 0 0 0
    // 0 0 0 0 ==> 2 0 0 0
    // 0 0 0 0 ==> 2 0 0 0
    // 0 0 0 0     2 0 0 0
    val m = Matrix(4).get
    val tilesToSeed = List((0, 0), (1, 0), (2, 0), (3, 0))
    val populatedMatrix = Array(
      Array(2, 0, 0, 0),
      Array(2, 0, 0, 0),
      Array(2, 0, 0, 0),
      Array(2, 0, 0, 0)
    )

    assert(Matrix.compareMatrices(m.populateMatrix(tilesToSeed),
                                  populatedMatrix))


  }

  test("populate matrix's first row when it's empty") (pending)

  test("populate matrix with content in it") (pending)

  test("populate matrix with more seeds than tiles") (pending)

  test("populate matrix when matrix is full") (pending)
}
