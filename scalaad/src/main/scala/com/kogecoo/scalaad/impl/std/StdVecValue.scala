package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.{BoolTensor1, Tensor1}


trait StdVecValue {

  implicit val value_stdvec_double: Value[Tensor1, StdVec[Double]] = new Value[Tensor1, StdVec[Double]] {
    def value(t: Tensor1): StdVec[Double] = t match {
      case t => t.toStdDouble
    }
  }

  implicit val value_stdtransvec_double: Value[Tensor1, StdTransVec[Double]] = new Value[Tensor1, StdTransVec[Double]] {
    def value(t: Tensor1): StdTransVec[Double] = t match {
      case t => t.toStdDouble.map(Seq(_))
    }
  }

  implicit val value_stdvec_boolean: Value[BoolTensor1, StdVec[Boolean]] = new Value[BoolTensor1, StdVec[Boolean]] {
    def value(t: BoolTensor1): StdVec[Boolean] = t match {
      case t => t.toStd
    }
  }

}
