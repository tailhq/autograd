package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad._
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.StdScalar
import org.nd4j.linalg.api.ndarray.INDArray

import scala.language.implicitConversions


trait Nd4jLeaf {

  object Var {

    def apply(data: Double): Var0 = Var0(StdScalar(data))
    def apply(data: INDArray): Node[_ <: Shape] = data.shape().size match {
      case 1 => Var1(Nd4jVector(data), Shape1(data.shape()(0)))
      case 2 => Var2(Nd4jMatrix(data), Shape2(data.rows(), data.columns()))
      case o => throw new UnsupportedTensorOrder(o)
    }

  }

  object Const {

    def apply(data: Double): Const0 = Const0(StdScalar(data))
    def apply(data: INDArray): Node[_ <: Shape] = data.shape().size match {
      case 1 => Const1(Nd4jVector(data), Shape1(data.shape()(0)))
      case 2 => Const2(Nd4jMatrix(data), Shape2(data.rows(), data.columns()))
      case o => throw new UnsupportedTensorOrder(o)
    }

  }

  implicit def fromByte(v: Byte):     Const0 = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const0 = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const0 = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const0 = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const0 = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const0 = Const(v)

  implicit def fromINDArray(v: INDArray): Node[_] = Const(v)

}

class UnsupportedTensorOrder(order: Int) extends Exception(s"Order ${order} tensor is not supported")
