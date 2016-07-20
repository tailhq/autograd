package scalaad.impl.breeze

import scalaad.graph.{Bool, Real}
import scalaad.{Tensor, Value}


trait BreezeValue {

  implicit val value_breeze_scalar_double: Value[Real, T0] = new Value[Real, T0] {
    def value(t: Tensor[Real]): T0 = t match {
      case t: BreezeScalar => t.data
    }
  }

  implicit val value_breeze_scalar_bool: Value[Bool, B0] = new Value[Bool, B0] {
    def value(t: Tensor[Bool]): B0 = t match {
      case t: BreezeBooleanScalar => t.data
    }
  }

  implicit val value_breeze_vector_double: Value[Real, T1] = new Value[Real, T1] {
    def value(t: Tensor[Real]): T1 = t match {
      case t: BreezeVector => t.data
    }
  }

  implicit val value_breeze_vector_bool: Value[Bool, B1] = new Value[Bool, B1] {
    def value(t: Tensor[Bool]): B1 = t match {
      case t: BreezeBooleanVector => t.data
    }
  }

  implicit val value_breeze_matrix_double: Value[Real, T2] = new Value[Real, T2] {
    def value(t: Tensor[Real]): T2 = t match {
      case t: BreezeMatrix => t.data
    }
  }

  implicit val value_breeze_matrix_bool: Value[Bool, B2] = new Value[Bool, B2] {
    def value(t: Tensor[Bool]): B2 = t match {
      case t: BreezeBooleanMatrix => t.data
    }
  }

  implicit val value_breeze_tensor3_double: Value[Real, T3] = new Value[Real, T3] {
    def value(t: Tensor[Real]): T3 = t match {
      case t: BreezeTensor3 => t.data
    }
  }

  implicit val value_breeze_tensor3_bool: Value[Bool, B3] = new Value[Bool, B3] {
    def value(t: Tensor[Bool]): B3 = t match {
      case t: BreezeBooleanTensor3 => t.data
    }
  }

}
