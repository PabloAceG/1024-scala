/** A matrix that contains the tails of the game.
 *
 * @param size Size of the NxN square matrix.
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
   * @return matrix with newly placed seeds.
   */
  def populateMatrix(tilesToSeed: List[(Int, Int)]):
      Array[Array[Int]] = {
    tilesToSeed match {
      case Nil => this.matrix
      case head :: tail => {
        val tileValue = (scala.util.Random.nextInt(2) * 2) + 2 // 2 or 4
        this.matrix(head._1)(head._2) = tileValue
        populateMatrix(tail)
      }
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
  // FIXME: Comparisons in case do not seem to be fine
  private def findAvailablePositions(x: Int,
                             y: Int): List[(Int, Int)] = {
    // Iterate through matrix.
    val edge = size - 1
    val coordinates = (x, y) match {
      case x == edge && y == edge => Nil
      case y == edge => findAvailablePositions(x + 1, 0)
      case _ => findAvailablePositions(x, y + 1)
    }

    // Append empty tile to list of free coordinates.
    if (this.matrix(x)(y) == 0) ((x, y) :: coordinates)
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
  // FIXME: Looks like the thingies do not exists
  private def positionsToSeed(seeds: Int,
                              freeTiles: List[(Int, Int)],
                              i: Int): List[Int] = {
    if (seeds == 0) Nil
    else {
      (freeTiles, i) match {
        case (ft.length < seeds, i) => positionsToSeed(ft.length, ft, i)
        case (ft.length >= seeds, -1) => {
          val coord = scala.util.Random.nextInt(ft.length - (s - 1))
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
  def compareMatrices(m1: Any, m2: Any): Boolean = {
    (m1, m2) match {
      case (a: Int, b: Int) => a == b
      case (headA :: tailA, headB :: tailB) =>
        compareMatrices(headA, headB) &&
        compareMatrices(tailB, tailB)
      case _ => true
    }
  }
}
