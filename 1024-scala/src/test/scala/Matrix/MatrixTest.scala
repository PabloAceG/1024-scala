package matrix

import org.scalatest.funsuite.AnyFunSuite

class MatrixTest extends AnyFunSuite {

  test ("create basic types matrices") {
    val mString = Matrix[String]()
    val mDouble = Matrix[Double]()
    val mFloat = Matrix[Float]()
    val mInt = Matrix[Int]()
    val mLong = Matrix[Float]()
    val mShort = Matrix[Byte]()
    val mBoolean = Matrix[Boolean]()

    assert(mString.isDefined && mDouble.isDefined && mFloat.isDefined
             && mInt.isDefined && mLong.isDefined && mShort.isDefined
             && mBoolean.isDefined)
  }

  test ("create collections (seq, set, map) matrices") {
    val mList = Matrix[List[Int]]()
    val mMap = Matrix[Map[Int, Int]]()
    val mSeq = Matrix[Seq[Int]]()
    val mSet = Matrix[Set[Int]]()
    assert(mList.isDefined && mMap.isDefined && mSeq.isDefined && mSet.isDefined)
  }

  test ("should not be possible to create an empty matrix") {
    val m = Matrix[Int](0, 0)
    assert(!m.isDefined)
  }

  test ("creating matrix using defualt constructor is a 4x4 matrix") {
    val m = Matrix[Int]().get
    assert(m.dimensions == (4, 4)
             && m.size == 16
             && m.toList == List(0, 0, 0, 0, 0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 ,0))
  }

  test ("creating matrix using size constructor returns square matrix - NxN") {
    val m = Matrix[Int](3).get
    assert(m.dimensions == (3, 3)
             && m.size == 9
             && m.toList == List(0, 0, 0, 0, 0, 0, 0, 0, 0))
  }

  test ("creating matrix using number cols/rows constructor gives NxM matrix") {
    val m = Matrix[Int](2, 4).get
    assert(m.dimensions == (2, 4)
             && m.size == 8
             && m.toList == List(0, 0, 0, 0, 0, 0, 0, 0))
  }

  test ("should not create matrix using list constructor without square-sized list") {
    val m = Matrix[Int](List(1, 2, 3))
    assert(!m.isDefined)
  }

  test ("creating matrix using list constructor with list size being square returns square matrix") {
    val m = Matrix[Int](List(1, 2, 3, 4)).get
    assert(m.dimensions == (2, 2)
             && m.size == 4
             && m.toList == List(1, 2, 3, 4))
  }

  test ("creating matrix using list and number cols/rows constructor returns NxM matrix") {
    val m = Matrix[Int](2, 2, List(1, 2, 3, 4)).get
    assert(m.dimensions == (2, 2)
             && m.size == 4
             && m.toList == List(1, 2, 3, 4))
  }

  test ("should not create matrix using list and number cols/rows different from list size") {
    val m = Matrix[Int](2, 3, List(1, 2, 3, 4))
    assert(!m.isDefined)
  }

  test ("convert square matrix to string") {
    val m = Matrix[Int](3, 3).get
    val mInStr = m.toString
    val correctStr = "[ [ 0 0 0 ] [ 0 0 0 ] [ 0 0 0 ] ]"
    assert(mInStr == correctStr)
  }

  test ("rectangular matrix to string") {
    val m = Matrix[Int](2, 3).get
    val mInStr = m.toString
    val correctStr = "[ [ 0 0 ] [ 0 0 ] [ 0 0 ] ]"
    assert(mInStr == correctStr)
  }

  test ("compare equal matrices") {
    val m1 = Matrix[Int](3, 3, List(1, 2, 3, 4, 5, 6, 7, 8, 9)).get
    val m2 = Matrix[Int](3, 3, List(1, 2, 3, 4, 5, 6, 7, 8, 9)).get
    assert(m1 == m2)
  }

  test ("compare matrices with different size") {
    val m1 = Matrix[Int](3, 3, List(1, 2, 3, 4, 5, 6, 7, 8, 9)).get
    val m2 = Matrix[Int](3, 4, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)).get
    assert(m1 != m2)
  }

  test ("compare matrices with different values") {
    val m1 = Matrix[Int](List(1, 2, 3, 4, 5, 6, 7, 8, 9)).get
    val m2 = Matrix[Int](List(1, 2, 3, 4, 5, 6, 7, 8, 0)).get
    assert(m1 != m2)
  }

  test ("transpose square matrix") {
    val m = Matrix[Int](List(1, 2, 3, 4, 5, 6, 7, 8, 9)).get
    val mTransposed = m.transposeMatrix()
    val correctMatrix = Matrix[Int](List(1, 4, 7, 2, 5, 8, 3, 6, 9)).get
    assert(mTransposed == correctMatrix)
  }

  test ("transpose rectangular matrix") {
    val m = Matrix[Int](4, 3, List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)).get
    val mTransposed = m.transposeMatrix()
    val correctMatrix = Matrix[Int](3, 4, List(1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)).get
    assert(mTransposed == correctMatrix)
  }

}
