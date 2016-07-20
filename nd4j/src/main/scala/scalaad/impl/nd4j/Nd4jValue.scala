package scalaad.impl.nd4j

import org.nd4j.linalg.api.ndarray.INDArray

import scalaad.{Tensor, Value}


trait Nd4jValue {

  implicit val value_nd4j_tensor_double: Value[INDArray] = {
    new Value[INDArray] {
      def value(t: Tensor): INDArray = t match {
        case t: Nd4jVector => t.data
      }
    }
  }

  implicit val value_nd4j_scalar_double: Value[T0] = {
    new Value[T0] {
      def value(t: Tensor): T0 = t match {
        case Nd4jScalar(data) => data
      }
    }
  }

  implicit val value_nd4j_tensor_bool: Value[INDArray] = {
    new Value[INDArray] {
      def value(t: Tensor): INDArray = t match {
        case t: Nd4jBooleanVector => t.data
      }
    }
  }

  implicit val value_nd4j_scalar_bool: Value[B0] = {
    new Value[B0] {

      def value(t: Tensor): B0 = t match {
        case Nd4jBooleanScalar(data) => data
      }
    }
  }

}
