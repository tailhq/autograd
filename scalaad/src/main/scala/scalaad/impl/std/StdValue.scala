package scalaad.impl.std

import scalaad.graph.{Bool, Real}
import scalaad.{Tensor, Value}


trait StdValue {

  implicit val value_std_scalar_double: Value[Real, T0] = {
    new Value[Real, T0] {
      def value(t: Tensor[Real]): T0 = t match {
        case a: StdScalar[T0, Real] @unchecked => a.data
      }
    }
  }

  implicit val value_std_vector_double: Value[Real, T1] = {
    new Value[Real, T1] {
      def value(t: Tensor[Real]): T1 = t match {
        case a: StdVector[T0, Real] @unchecked => a.data
      }
    }
  }

  implicit val value_std_matrix_double: Value[Real, T2] = {
    new Value[Real, T2] {

      def value(t: Tensor[Real]): T2 = t match {
        case a: StdMatrix[T0, Real] @unchecked => a.data
      }

    }
  }


  implicit val value_std_scalar_bool: Value[Bool, B0] = {
    new Value[Bool, B0] {

      def value(t: Tensor[Bool]): B0 = t match {
        case a: StdScalar[B0, Bool] @unchecked => a.data
      }
    }
  }

  implicit val value_std_vector_bool: Value[Bool, B1] = {
    new Value[Bool, B1] {

      def value(t: Tensor[Bool]): B1 = t match {
        case a: StdVector[B0, Bool] @unchecked => a.data
      }
    }
  }

  implicit val value_std_matrix_bool: Value[Bool, B2] = {
    new Value[Bool, B2] {

      def value(t: Tensor[Bool]): B2 = t match {
        case a: StdMatrix[B0, Bool] @unchecked => a.data

      }
    }
  }

}
