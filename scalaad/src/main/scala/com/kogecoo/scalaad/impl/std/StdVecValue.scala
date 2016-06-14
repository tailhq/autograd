package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Value
import com.kogecoo.scalaad.{BooleanTensor, BooleanTensor1, Tensor}
import shapeless.Nat._1


trait StdVecValue {

  implicit val value_stdvec_double: Value[Tensor[_1], T1] = {
    new Value[Tensor[_1], T1] {

      def value(t: Tensor[_1]): T1 = t match {
        case StdVector(data) => data
      }
    }
  }

  implicit val value_stdvec_boolean: Value[BooleanTensor[_1], Vec[Boolean]] = {
    new Value[BooleanTensor[_1], Vec[Boolean]] {

      def value(t: BooleanTensor[_1]): Vec[Boolean] = t match {
        case b: BooleanTensor1 => b.toStd
      }
    }
  }

}
