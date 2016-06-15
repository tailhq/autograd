package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BooleanTensor, BooleanValue, Tensor, Value}
import shapeless.Nat._0


trait StdScalarValue {

  implicit val value_double: Value[_0, T0] = {
    new Value[_0, T0] {

      def value(t: Tensor[_0]): T0 = t match {
        case StdScalar(data) => data
      }
    }
  }

  implicit val value_boolean: BooleanValue[_0, Boolean] = {
    new BooleanValue[_0, Boolean] {

      def value(t: BooleanTensor[_0]): Boolean = t match {
        case StdBooleanScalar(data) => data
      }
    }
  }

}
