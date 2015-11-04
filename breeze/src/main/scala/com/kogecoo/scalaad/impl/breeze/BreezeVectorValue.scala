package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.{Transpose, BitVector, DenseVector}
import com.kogecoo.scalaad.{BoolTensor1, Tensor1}
import com.kogecoo.scalaad.algorithm.Value

trait BreezeVectorValue {

  implicit val value_breeze_vector_double: Value[Tensor1, DenseVector[Double]] = new Value[Tensor1, DenseVector[Double]] {
    def value(t: Tensor1): DenseVector[Double] = t match {
      case t: BreezeVector => t.data
    }
  }

  implicit val value_breeze_trans_vector_double: Value[Tensor1, Transpose[DenseVector[Double]]] = new Value[Tensor1, Transpose[DenseVector[Double]]] {
    def value(t: Tensor1): Transpose[DenseVector[Double]] = t match {
      case t: BreezeVector => t.data.t
    }
  }

  implicit val value_breeze_vector_boolean: Value[BoolTensor1, BitVector] = new Value[BoolTensor1, BitVector] {
    def value(t: BoolTensor1): BitVector = t match {
      case t: BreezeBooleanVector => t.data
    }
  }

}
