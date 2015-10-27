package com.kogecoo.scalaad.breeze.test.helper.gen

import breeze.linalg.DenseMatrix
import org.scalacheck.Gen


object DenseMatrixGen {

  def genDenseMatrix(shape: MatrixShape, min: Double, max: Double, restrict: Double => Boolean): Gen[DenseMatrix[Double]] = {
    for {
      data <- Gen.containerOfN[Array, Double](shape.rows * shape.cols, Gen.choose[Double](min, max) suchThat restrict)
    } yield new DenseMatrix[Double](shape.rows, shape.cols, data)
  }

}
