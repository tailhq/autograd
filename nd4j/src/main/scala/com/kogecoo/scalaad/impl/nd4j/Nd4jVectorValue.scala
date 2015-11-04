package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.{BoolTensor1, Tensor1}
import com.kogecoo.scalaad.algorithm.Value
import org.nd4j.linalg.api.ndarray.INDArray


trait Nd4jVectorValue {

  implicit val value_nd4j_vector_double: Value[Tensor1, INDArray] = new Value[Tensor1, INDArray] {
    def value(t: Tensor1): INDArray = t match {
      case t: Nd4jVector => t.data
    }
  }

  implicit val value_nd4j_vector_boolean: Value[BoolTensor1, INDArray] = new Value[BoolTensor1, INDArray] {
    def value(t: BoolTensor1): INDArray = t match {
      case t: Nd4jBooleanVector => t.data
    }
  }

}
