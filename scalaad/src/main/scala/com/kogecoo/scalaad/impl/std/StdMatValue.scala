package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BooleanTensor, BooleanValue, Tensor, Value}
import shapeless.Nat._2


trait StdMatValue {

  implicit val value_stdmat_double: Value[_2, T2] = {
    new Value[_2, T2] {

      def value(t: Tensor[_2]): T2 = t match {
        case StdMatrix(data) => data
      }

    }
  }

  implicit val value_stdmat_boolean: BooleanValue[_2, Mat[Boolean]] = {
    new BooleanValue[_2, Mat[Boolean]] {

      def value(t: BooleanTensor[_2]): Mat[Boolean] = t match {
        case StdBooleanMatrix(data) => data

      }
    }
  }

}
