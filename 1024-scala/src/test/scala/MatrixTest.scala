import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.PrivateMethodTester

class MatrixTest extends AnyFunSuite with PrivateMethodTester {

  test("comparing two equal matrices (all 0s) should be fine") (pending)

  test("comparing two equal matrices (\"ramdom\" pattern) should be fine") (pending)

  test("comparing two different matrices does not pass") (pending)

  test("should not compare matrices of different sizes") (pending)

  test("transforms an array to a formatted string") {
    val arr = Array(1, 2, 3, 4, 5)
    val arrToStr = "[1 2 3 4 5]"
    assert(Matrix.matrixToString(arr) == arrToStr)
  }

  test("transforms a matrix to a formatted string") {
    val matrix = Array(
      Array(1, 2, 3, 4, 5),
      Array(6, 7, 8, 9)
    )
    val matrixToStr = "[[1 2 3 4 5] [6 7 8 9]]"
    assert(Matrix.matrixToString(matrix) == matrixToStr)
  }

  test("matrix smaller than 4 cannot be created") {
    val m = Matrix(3)
    assert(!m.isDefined)
  }

  test("able to create matrix object within min-size and max-size") {
    for (i <- Matrix.MINSIZE to Matrix.MAXSIZE) {
      val m = Matrix(i)
      assert(m.isDefined)
    }
  }

  test("matrix with size greater than 10 cannot be created") {
    val m = Matrix(100)
    assert(!m.isDefined)
  }

  test("matrix going from min-size to max-size can be initialized") {
    for (i <- Matrix.MINSIZE to Matrix.MAXSIZE) {
      val m = Matrix(i).get.matrix
      val testMatrix = Array.fill(i){Array.fill(i){0}}

      // TODO
      // assert(Matrix.compareMatrices(m, testMatrix))
    }
  }

  test("number of seeds relates to matrix's size") {
    val testExpectedSeeds = Vector(2, 2, 3, 3, 4, 4, 5)
    // Private methods cannot usually be accessed outside class. Need this:
    val numberSeedsOnSize = PrivateMethod[Int]('numberSeedsOnSize)
    val seeds = for(i <- Matrix.MINSIZE to Matrix.MAXSIZE)
      yield(Matrix(i).get invokePrivate numberSeedsOnSize())
    assert(seeds == testExpectedSeeds)
  }

  test("find positions to seed with no tile in the matrix") {
     val m = Array(
       Array(0, 0, 0, 0),
       Array(0, 0, 0, 0),
       Array(0, 0, 0, 0),
       Array(0, 0, 0, 0))
    val findAvailablePositions =
      PrivateMethod[List[(Int, Int)]]('findAvailablePositions)
    val emptyTiles = Matrix(4).get invokePrivate findAvailablePositions(m, 0, 0)
    val expectedEmptyTiles = List((0, 0), (0, 1), (0, 2), (0, 3),
                                  (1, 0), (1, 1), (1, 2), (1, 3),
                                  (2, 0), (2, 1), (2, 2), (2, 3),
                                  (3, 0), (3, 1), (3, 2), (3, 3))
    assert(emptyTiles == expectedEmptyTiles)
  }

  test("find positions to seed with tiles in the matrix") {
     val m = Array(
       Array(1, 0, 0, 0),
       Array(0, 1, 0, 0),
       Array(0, 0, 1, 0),
       Array(0, 0, 0, 1))
    val findAvailablePositions =
      PrivateMethod[List[(Int, Int)]]('findAvailablePositions)
    val emptyTiles = Matrix(4).get invokePrivate findAvailablePositions(m, 0, 0)

    val expectedEmptyTiles = List(        (0, 1), (0, 2), (0, 3),
                                  (1, 0),         (1, 2), (1, 3),
                                  (2, 0), (2, 1),         (2, 3),
                                  (3, 0), (3, 1), (3, 2))

    assert(emptyTiles == expectedEmptyTiles)
  }

  test("select positions to seed") (pending)

  test("populate matrix's first column when it's empty") {
    // 0 0 0 0     2 0 0 0
    // 0 0 0 0 ==> 2 0 0 0
    // 0 0 0 0 ==> 2 0 0 0
    // 0 0 0 0     2 0 0 0
    for (i <- Matrix.MINSIZE to Matrix.MAXSIZE) {
      val m = Matrix(i).get
      val tilesToSeed = List.fill(i)((i - Matrix.MINSIZE, 0))
      m.populateMatrix(tilesToSeed)
      val populatedMatrix = Array.fill(i){2 +: Array.fill(i - 1){0}}

      println(Matrix.matrixToString(m.matrix))
      println(Matrix.matrixToString(populatedMatrix))
      println("----------------")

      // TODO
      // assert(Matrix.compareMatrices(m.matrix, populatedMatrix))
    }
  }

  // TODO: Adapt to all accepted sizes
  test("populate matrix's first row when it's empty") {
    // 0 0 0 0     2 2 2 2
    // 0 0 0 0 ==> 0 0 0 0
    // 0 0 0 0 ==> 0 0 0 0
    // 0 0 0 0     0 0 0 0
    val m = Matrix(4).get
    val tilesToSeed = List((0, 0), (0, 1), (0, 2), (0, 3))
    val populatedMatrix = Array(
      Array(2, 2, 2, 2),
      Array(0, 0, 0, 0),
      Array(0, 0, 0, 0),
      Array(0, 0, 0, 0)
    )

    // TODO
    // assert(Matrix.compareMatrices(m.populateMatrix(tilesToSeed), populatedMatrix))
  }

  test("populate matrix with content in it") (pending)

  test("populate matrix with more seeds than tiles") (pending)

  test("populate matrix when matrix is full") (pending)
}
