package com.kogecoo.scalaad.breeze.test.helper.gen

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.test.helper.gen._
import com.kogecoo.scalaad.value.{ContainerValue, NonContainerValue}
import org.scalacheck.Gen

import scala.language.higherKinds


class DenseMatrixValueGen(min: Double, max: Double) extends ShapeRestrictedValueGen[DenseMatrix, Double] {

  override def genNonContainerValueWithSource(restrict: Double => Boolean): Gen[NonContainerValueSample[DenseMatrix, Double]] = {
    for {
      v <- Gen.choose(min, max) suchThat restrict
    } yield new NonContainerValueSample[DenseMatrix, Double](NonContainerValue[DenseMatrix, Double](v), v)
  }

  override def genContainerValueWithSource(shape: MatrixShape, restrict: Double => Boolean): Gen[ContainerValueSample[DenseMatrix, Double]] = {
    for {
      m <- DenseMatrixGen.genDenseMatrix(shape, min, max, restrict)
    } yield new ContainerValueSample[DenseMatrix, Double](ContainerValue[DenseMatrix, Double](m), m)
  }

}
