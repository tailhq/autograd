package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.{BooleanTensor1, Tensor1}


trait StdVecValue {

  implicit val value_stdvec_double: Value[Tensor1, T1] = new Value[Tensor1, T1] {
    def value(t: Tensor1): T1 = t match {
      case t => t.toStdDouble
    }
  }

  implicit val value_stdvec_boolean: Value[BooleanTensor1, Vec[Boolean]] = new Value[BooleanTensor1, Vec[Boolean]] {
    def value(t: BooleanTensor1): Vec[Boolean] = t match {
      case t => t.toStd
    }
  }

}
