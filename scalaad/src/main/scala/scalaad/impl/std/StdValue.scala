package scalaad.impl.std

import scalaad.{Tensor, Value}


trait StdValue {

  implicit val value_std_scalar_double: Value[T0] = {
    new Value[T0] {
      def value(t: Tensor): T0 = t match {
        case StdScalar(data) => data
      }
    }
  }

  implicit val value_std_vector_double: Value[T1] = {
    new Value[T1] {
      def value(t: Tensor): T1 = t match {
        case StdVector(data) => data
      }
    }
  }

  implicit val value_std_matrix_double: Value[T2] = {
    new Value[T2] {

      def value(t: Tensor): T2 = t match {
        case StdMatrix(data) => data
      }

    }
  }


  implicit val value_std_scalar_bool: Value[B0] = {
    new Value[B0] {

      def value(t: Tensor): B0 = t match {
        case StdBooleanScalar(data) => data
      }
    }
  }

  implicit val value_std_vector_bool: Value[B1] = {
    new Value[B1] {

      def value(t: Tensor): B1 = t match {
        case StdBooleanVector(data) => data
      }
    }
  }

  implicit val value_std_matrix_bool: Value[B2] = {
    new Value[B2] {

      def value(t: Tensor): B2 = t match {
        case StdBooleanMatrix(data) => data

      }
    }
  }

}
