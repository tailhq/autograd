package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.{BooleanTensor2, Tensor2}


trait StdMatValue {

  implicit val value_stdmat_double: Value[Tensor2, T2] = new Value[Tensor2, T2] {
    def value(t: Tensor2): T2 = t match {
      case t => t.toStdDouble
    }
  }

  implicit val value_stdmat_boolean: Value[BooleanTensor2, Mat[Boolean]] = new Value[BooleanTensor2, Mat[Boolean]] {
    def value(t: BooleanTensor2): Mat[Boolean] = t match {
      case t => t.toStd
    }
  }
}
