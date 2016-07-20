package scalaad.impl.std

import scalaad.{BooleanTensor, BooleanValue, Tensor, Value}


trait StdVecValue {

  implicit val value_std_vector_double: Value[T1] = {
    new Value[T1] {
      def value(t: Tensor): T1 = t match {
        case StdVector(data) => data
      }
    }
  }

  implicit val value_std_vector_bool: BooleanValue[Vec[Boolean]] = {
    new BooleanValue[Vec[Boolean]] {

      def value(t: BooleanTensor): Vec[Boolean] = t match {
        case StdBooleanVector(data) => data
      }
    }
  }

}
