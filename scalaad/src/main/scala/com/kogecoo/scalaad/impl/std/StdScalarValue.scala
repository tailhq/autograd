package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.{BooleanTensor, BooleanTensor0, Tensor}
import shapeless.Nat._0


trait StdScalarValue {

  implicit val value_double: Value[Tensor[_0], Double] = {
    new Value[Tensor[_0], Double] {

      def value(t: Tensor[_0]): Double = t match {
        case StdScalar(data) => data
      }
    }
  }

  implicit val value_boolean: Value[BooleanTensor[_0], Boolean] = {
    new Value[BooleanTensor[_0], Boolean] {

      def value(t: BooleanTensor[_0]): Boolean = t match {
        case b: BooleanTensor0 => b.toStd
      }
    }
  }

}
