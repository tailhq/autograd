package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.{BooleanTensor, BooleanTensor2, Tensor, Value}
import shapeless.Nat._2


trait StdMatValue {

  implicit val value_stdmat_double: Value[Tensor[_2], T2] = {
    new Value[Tensor[_2], T2] {

      def value(t: Tensor[_2]): T2 = t match {
        case StdMatrix(data) => data
      }

    }
  }

  implicit val value_stdmat_boolean: Value[BooleanTensor[_2], Mat[Boolean]] = {
    new Value[BooleanTensor[_2], Mat[Boolean]] {

      def value(t: BooleanTensor[_2]): Mat[Boolean] = t match {
        case b: BooleanTensor2 => b.toStd

      }
    }
  }

}
