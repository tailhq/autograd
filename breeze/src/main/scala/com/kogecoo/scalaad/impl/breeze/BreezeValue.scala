package com.kogecoo.scalaad.impl.breeze

import com.kogecoo.scalaad.{BooleanTensor, BooleanValue, Tensor, Value}


trait BreezeValue {

  implicit val value_breeze_scalar_double: Value[T0] = new Value[T0] {
    def value(t: Tensor): T0 = t match {
      case t: BreezeScalar => t.data
    }
  }

  implicit val value_breeze_scalar_bool: BooleanValue[B0] = new BooleanValue[B0] {
    def value(t: BooleanTensor): B0 = t match {
      case t: BreezeBooleanScalar => t.data
    }
  }

  implicit val value_breeze_vector_double: Value[T1] = new Value[T1] {
    def value(t: Tensor): T1 = t match {
      case t: BreezeVector => t.data
    }
  }

  implicit val value_breeze_vector_bool: BooleanValue[B1] = new BooleanValue[B1] {
    def value(t: BooleanTensor): B1 = t match {
      case t: BreezeBooleanVector => t.data
    }
  }

  implicit val value_breeze_matrix_double: Value[T2] = new Value[T2] {
    def value(t: Tensor): T2 = t match {
      case t: BreezeMatrix => t.data
    }
  }

  implicit val value_breeze_matrix_bool: BooleanValue[B2] = new BooleanValue[B2] {
    def value(t: BooleanTensor): B2 = t match {
      case t: BreezeBooleanMatrix => t.data
    }
  }

  implicit val value_breeze_tensor3_double: Value[T3] = new Value[T3] {
    def value(t: Tensor): T3 = t match {
      case t: BreezeTensor3 => t.data
    }
  }

  implicit val value_breeze_tensor3_bool: BooleanValue[B3] = new BooleanValue[B3] {
    def value(t: BooleanTensor): B3 = t match {
      case t: BreezeBooleanTensor3 => t.data
    }
  }

}
