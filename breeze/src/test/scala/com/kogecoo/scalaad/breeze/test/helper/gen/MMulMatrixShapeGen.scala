package com.kogecoo.scalaad.breeze.test.helper.gen

import org.scalacheck.Gen


case class MatrixShape(rows: Int, cols: Int)

class MMulMatrixShapeGen(minRow: Int, minCol: Int, maxRow: Int, maxCol: Int) {

  def genShape(): Gen[(MatrixShape, MatrixShape)] = {
    for {
      rows1 <- Gen.choose[Int](minRow, maxRow)
      cols1 <- Gen.choose[Int](minCol, maxCol)
      cols2 <- Gen.choose[Int](minCol, maxCol)
    } yield (MatrixShape(rows1, cols1), MatrixShape(cols1, cols2))
  }

}


