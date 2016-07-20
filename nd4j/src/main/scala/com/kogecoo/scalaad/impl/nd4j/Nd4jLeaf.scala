package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.graph.{Expr, Expr$, Const => Const_, Var => Var_}
import org.nd4j.linalg.api.ndarray.INDArray

import scala.language.implicitConversions


trait Nd4jLeaf {

  object Var {

    def apply(data: T0): Var_       = Var_(Nd4jScalar(data))
    def apply(data: INDArray): Var_ = Var_(Nd4jTensor(data))

  }

  object Const {

    def apply(data: T0): Const_       = Const_(Nd4jScalar(data))
    def apply(data: INDArray): Const_ = Const_(Nd4jTensor(data))

  }

  implicit def fromByte(v: Byte):     Const_ = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const_ = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const_ = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const_ = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const_ = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const_ = Const(v)

  implicit def fromINDArray(v: INDArray): Expr = Const(v)

}

