package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.DenseMatrix
import com.kogecoo.scalaad.{BoolTensor2, Tensor2}
import com.kogecoo.scalaad.algorithm.Value


trait BreezeMatrixValue {

  implicit val value_breeze_matrix_double: Value[Tensor2, DenseMatrix[Double]] = new Value[Tensor2, DenseMatrix[Double]] {
    def value(t: Tensor2): DenseMatrix[Double] = t match {
      case t: BreezeMatrix => t.data
    }
  }

  implicit val value_breeze_matrix_boolean: Value[BoolTensor2, DenseMatrix[Boolean]] = new Value[BoolTensor2, DenseMatrix[Boolean]] {
    def value(t: BoolTensor2): DenseMatrix[Boolean] = t match {
      case t: BreezeBooleanMatrix => t.data
    }
  }

}
