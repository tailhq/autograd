package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BooleanTensor, BooleanValue, Tensor, Value}
import shapeless.Nat._1


trait StdVecValue {

  implicit val value_stdvec_double: Value[_1, T1] = {
    new Value[_1, T1] {

      def value(t: Tensor[_1]): T1 = t match {
        case StdVector(data) => data
      }
    }
  }

  implicit val value_stdvec_boolean: BooleanValue[_1, Vec[Boolean]] = {
    new BooleanValue[_1, Vec[Boolean]] {

      def value(t: BooleanTensor[_1]): Vec[Boolean] = t match {
        case StdBooleanVector(data) => data
      }
    }
  }

}
