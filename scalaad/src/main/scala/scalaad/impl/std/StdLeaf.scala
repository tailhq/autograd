package scalaad.impl.std

import scalaad.graph.{Real, Const => Const_, Var => Var_}
import scala.language.implicitConversions


trait StdLeaf {

  object Var {

    def apply(data: Scalar[T0]): Var_ = Var_(StdScalar(data))
    def apply(data: Vec[T0])(implicit d: DummyImplicit): Var_ = Var_(StdVector(data))
    def apply(data: Mat[T0])(implicit d1: DummyImplicit, d2: DummyImplicit): Var_ = Var_(StdMatrix(data))

  }

  object Const {

    def apply(data: Scalar[T0]): Const_[Real] = Const_[Real](StdScalar(data))
    def apply(data: Vec[T0]): Const_[Real] = Const_[Real](StdVector(data))
    def apply(data: Mat[T0])(implicit d: DummyImplicit): Const_[Real] = Const_(StdMatrix(data))

  }

  implicit def fromByte(v: Byte):     Const_[Real] = Const(v.toFloat)
  implicit def fromShort(v: Short):   Const_[Real] = Const(v.toFloat)
  implicit def fromInt(v: Int):       Const_[Real] = Const(v.toFloat)
  implicit def fromLong(v: Long):     Const_[Real] = Const(v.toFloat)
  implicit def fromDouble(v: Double): Const_[Real] = Const(v.toFloat)
  implicit def fromFloat(v: Float):   Const_[Real] = Const(v)

  implicit def fromVecByte(v: Vec[Byte]):   Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromVecShort(v: Vec[Short]): Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromVecInt(v: Vec[Int]):     Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromVecLong(v: Vec[Long]):   Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromVecFloat(v: Vec[Float]): Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromVec(v: Vec[Double]):     Const_[Real] = Const(v)

  implicit def fromMatByte(v: Mat[Byte]):   Const_[Real] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatShort(v: Mat[Short]): Const_[Real] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatInt(v: Mat[Int]):     Const_[Real] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatLong(v: Mat[Long]):   Const_[Real] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatFloat(v: Mat[Float]): Const_[Real] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMat(v: Mat[Double]):     Const_[Real] = Const(v)

}
