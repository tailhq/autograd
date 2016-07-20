package scalaad.impl.std

import scalaad.{BooleanTensor, BooleanValue, Tensor, Value}


trait StdMatValue {

  implicit val value_std_matrix_double: Value[T2] = {
    new Value[T2] {

      def value(t: Tensor): T2 = t match {
        case StdMatrix(data) => data
      }

    }
  }

  implicit val value_std_matrix_bool: BooleanValue[Mat[Boolean]] = {
    new BooleanValue[Mat[Boolean]] {

      def value(t: BooleanTensor): Mat[Boolean] = t match {
        case StdBooleanMatrix(data) => data

      }
    }
  }

}
