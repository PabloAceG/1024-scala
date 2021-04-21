/** A matrix that contains the tails of the game.
 *
 * @constructor create a new matrix with a size.
 * @param size size of the NxN square matrix.
 */
class Matrix(val size: Int) {
  /** Square matrix containing the number tiles of the game. */
  var matrix = Array[Array[Int]]()
  /** Points accumulated during the game */
  var points = 0

  /** Initialize the NxN square matrix with 0 in all tiles.
   *
   * @return a new Matrix containing all tiles set to 0.
   */
  def initMatrix(): Array[Array[Int]] = {
    var m = Array.fill(size, size)(0)
    this.matrix = m
    m
  }

  /** Populates the matrix with new seeds at the selected positions.
   *
   * @param positions where to place the new tiles.
   *
   * @return matrix with newly placed seeds.
   */
  def populateMatrix(matrix: Array[Array[Int]],
                     tilesToSeed: List[(Int, Int, Int)]):
      Array[Array[Int]] = tilesToSeed match {
    case Nil => matrix
    case head :: tail => {
      matrix(head._1)(head._2) = head._3
      populateMatrix(matrix, tail)
    }
  }

  /** Give the number of seeds to insert depending on the size of the matrix.
   *
   * @return number of seeds depending on matrix's size.
   */
  private def numberSeedsOnSize(): Int = this.size / 2


  /** Find the free tiles in the square matrix and returns them as a list.
   *
   * @param x coordinate x in the bi-dimensional matrix.
   * @param y coordinate y in the bi-dimensional matrix.
   *
   * @return list of free coordinates where to insert a tile.
   */
  private def findAvailablePositions(m: Array[Array[Int]],
                                     x: Int,
                                     y: Int): List[(Int, Int)] = {
    // Iterate through matrix.
    val edge = size - 1
    val coordinates = (x, y) match {
      case (x, y) if x == edge && y == edge => Nil
      case (_, y) if y == edge => findAvailablePositions(m, x + 1, 0)
      case _ => findAvailablePositions(m, x, y + 1)
    }

    // Append empty tile to list of free coordinates.
    if (m(x)(y) == 0) ((x, y) :: coordinates)
    else coordinates
  }

  /** Select positions where new seeds are going to be created. Returns the
   *  positions in order.
   *
   * @param seeds number of sides to create (tiles to occupy).
   * @param freeTiles list containing the coordinates of the tales with no value
   * @param i iterator, used internally to know when to collect a seed. For
   *          external usage, call with passing 0.
   *
   * @return list of tiles to occupy.
   */
  // TODO: Also place seed, might need to change name of function
  private def positionsToSeed(seeds: Int,
                              freeTiles: List[(Int, Int)],
                              i: Int): List[(Int, Int)] = {
    if (seeds == 0) Nil
    else {
      (freeTiles, i) match {
        case (ft, i) if (ft.length < seeds) => positionsToSeed(ft.length, ft, i)
        case (ft, -1) if (ft.length >= seeds) => {
          val coord = scala.util.Random.nextInt(ft.length - (seeds - 1))
          positionsToSeed(seeds, ft, coord)
        }
        case (head :: tail, 0) => head :: positionsToSeed(seeds - 1, tail, -1)
        case (head :: tail, i) => positionsToSeed(seeds, tail, i - 1)
      }
    }
  }
}


/** Matrix object. */
object Matrix {
  /** Size limitations */
  val MINSIZE = 4
  val MAXSIZE = 10

  // TODO: Could create custom exception
  def apply(size: Int): Option[Matrix] = {
    try {
      if (MINSIZE <= size && size <= MAXSIZE) {
        val m = new Matrix(size)
        m.initMatrix()
        Some(m)
      } else {
        throw new Exception
      }

    } catch {
      case e: Exception => None
    }
  }

  /** Compares two matrices of integers.
   *
   * @param m1 NxN integer matrix.
   * @param m2 NxN integer matrix to compare to m1.
   *
   * @return True if matrices have same size and have same values. False
   *         otherwise.
   */
  // TODO: Add comprobation for size
  def compareMatrices[A](m1: Array[Array[Int]],
                         m2: Array[Array[Int]]): Boolean = {
    val flatMatrix1 = m1.flatMap(_.toArray)
    val flatMatrix2 = m2.flatMap(_.toArray)
    flatMatrix1
      .zip(flatMatrix2)
      .map{case (a, b) => a == b}
      .reduce((x0, x1) => x0 && x1)
  }

  /** Transforms an array into an string output, even if it is multi-dimesional.
   *
   * @param array to be transformed to strig.
   *
   * @return array transformed to string.
   */
  def matrixToString(x: Any): String = x match {
    case arr: Array[_] => arr.map(matrixToString).mkString("["," ","]")
    case _ => x.toString
  }
}
