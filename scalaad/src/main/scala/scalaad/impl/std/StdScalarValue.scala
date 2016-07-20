package scalaad.impl.std

import scalaad.{BooleanTensor, BooleanValue, Tensor, Value}


trait StdScalarValue {

  implicit val value_std_scalar_double: Value[T0] = {
    new Value[T0] {
      def value(t: Tensor): T0 = t match {
        case StdScalar(data) => data
      }
    }
  }

  implicit val value_std_scalar_bool: BooleanValue[Boolean] = {
    new BooleanValue[Boolean] {

      def value(t: BooleanTensor): Boolean = t match {
        case StdBooleanScalar(data) => data
      }
    }
  }

}
