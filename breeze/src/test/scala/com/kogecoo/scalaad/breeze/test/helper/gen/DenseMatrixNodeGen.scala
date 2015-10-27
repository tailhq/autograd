package com.kogecoo.scalaad.breeze.test.helper.gen

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.breeze.BreezeRule.Implicits._
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.test.helper.gen._
import org.scalacheck.Gen


class DenseMatrixNodeGen(min: Double, max: Double) extends ShapeRestrictedNodeGen[DenseMatrix, Double] {

  override def genScalarConstWithSource(restrict: Double => Boolean): Gen[ScalarConstSample[DenseMatrix, Double]] = {
    for {
      v <- Gen.choose(min, max) suchThat restrict
    } yield new ScalarConstSample[DenseMatrix, Double](ScalarConst[DenseMatrix, Double](v), v)
  }

  override def genContainerConstWithSource(shape: MatrixShape, restrict: Double => Boolean): Gen[ContainerConstSample[DenseMatrix, Double]] = {
    for {
      m <- DenseMatrixGen.genDenseMatrix(shape, min, max, restrict)
    } yield new ContainerConstSample[DenseMatrix, Double](ContainerConst[DenseMatrix, Double](m), m)
  }

 override def genVarWithSource(shape: MatrixShape, restrict: Double=> Boolean): Gen[VarSample[DenseMatrix, Double]] = {
    for {
      m <- DenseMatrixGen.genDenseMatrix(shape, min, max, restrict)
    } yield new VarSample[DenseMatrix, Double](Var[DenseMatrix, Double](m), m)
  }

}

