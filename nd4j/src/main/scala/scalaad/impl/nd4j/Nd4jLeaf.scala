package scalaad.impl.nd4j

import org.nd4j.linalg.api.ndarray.INDArray

import scala.language.implicitConversions
import scalaad.graph.{Expr, Real, Const => Const_, Var => Var_}


trait Nd4jLeaf {

  object Var {

    def apply(data: T0): Var_       = Var_(Nd4jScalar(data))
    def apply(data: INDArray): Var_ = Var_(Nd4jTensor(data))

  }

  object Const {

    def apply(data: T0): Const_[Real]       = Const_(Nd4jScalar(data))
    def apply(data: INDArray): Const_[Real] = Const_(Nd4jTensor(data))

  }

  implicit def fromByte(v: Byte):     Const_[Real] = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const_[Real] = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const_[Real] = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const_[Real] = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const_[Real] = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const_[Real] = Const(v)

  implicit def fromINDArray(v: INDArray): Expr[Real] = Const(v)

}
