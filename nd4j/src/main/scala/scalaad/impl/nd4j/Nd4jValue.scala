package scalaad.impl.nd4j

import org.nd4j.linalg.api.ndarray.INDArray

import scalaad.graph.{Bool, Real}
import scalaad.{Tensor, Value}


trait Nd4jValue {

  implicit val value_nd4j_tensor_double: Value[Real, INDArray] = {
    new Value[Real, INDArray] {
      def value(t: Tensor[Real]): INDArray = t match {
        case t: Nd4jVector => t.data
      }
    }
  }

  implicit val value_nd4j_scalar_double: Value[Real, T0] = {
    new Value[Real, T0] {
      def value(t: Tensor[Real]): T0 = t match {
        case Nd4jScalar(data) => data
      }
    }
  }

  implicit val value_nd4j_tensor_bool: Value[Bool, INDArray] = {
    new Value[Bool, INDArray] {
      def value(t: Tensor[Bool]): INDArray = t match {
        case t: Nd4jBooleanVector => t.data
      }
    }
  }

  implicit val value_nd4j_scalar_bool: Value[Bool, B0] = {
    new Value[Bool, B0] {

      def value(t: Tensor[Bool]): B0 = t match {
        case Nd4jBooleanScalar(data) => data
      }
    }
  }

}
