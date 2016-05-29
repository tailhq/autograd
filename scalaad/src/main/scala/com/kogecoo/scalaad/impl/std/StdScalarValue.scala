package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BooleanTensor0, Tensor0}
import com.kogecoo.scalaad.algorithm.Value


trait StdScalarValue {

  implicit val value_double: Value[Tensor0, Double] = new Value[Tensor0, Double] {
    def value(t: Tensor0): Double = t match {
      case t => t.toStdDouble
    }
  }

  implicit val value_boolean: Value[BooleanTensor0, Boolean] = new Value[BooleanTensor0, Boolean] {
    def value(t: BooleanTensor0): Boolean = t match {
      case t => t.toStd
    }
  }

}
