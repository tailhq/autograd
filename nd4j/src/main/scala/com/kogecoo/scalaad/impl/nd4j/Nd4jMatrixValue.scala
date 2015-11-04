package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.{BoolTensor2, Tensor2}
import com.kogecoo.scalaad.algorithm.Value
import org.nd4j.linalg.api.ndarray.INDArray


trait Nd4jMatrixValue {

  implicit val value_nd4j_matrix_double: Value[Tensor2, INDArray] = new Value[Tensor2, INDArray] {
    def value(t: Tensor2): INDArray = t match {
      case t: Nd4jMatrix => t.data
    }
  }

  implicit val value_nd4j_matrix_boolean: Value[BoolTensor2, INDArray] = new Value[BoolTensor2, INDArray] {
    def value(t: BoolTensor2): INDArray = t match {
      case t: Nd4jBooleanMatrix => t.data
    }
  }

}
