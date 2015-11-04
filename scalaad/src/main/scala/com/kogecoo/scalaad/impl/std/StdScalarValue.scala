package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BoolTensor0, Tensor0}
import com.kogecoo.scalaad.algorithm.Value


trait StdScalarValue {

  implicit val value_double: Value[Tensor0, Double] = new Value[Tensor0, Double] {
    def value(t: Tensor0): Double = t match {
      case t => t.toStdDouble
    }
  }

  implicit val value_boolean: Value[BoolTensor0, Boolean] = new Value[BoolTensor0, Boolean] {
    def value(t: BoolTensor0): Boolean = t match {
      case t => t.toStd
    }
  }

}
