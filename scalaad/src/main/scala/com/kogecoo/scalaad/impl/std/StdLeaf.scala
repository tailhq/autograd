package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.graph.{Const => Const_, Var => Var_}

import scala.language.implicitConversions


trait StdLeaf {

  object Var {

    def apply(data: T0): Var_ = Var_(StdScalar(data))
    def apply(data: T1)(implicit d: DummyImplicit): Var_ = Var_(StdVector(data))
    def apply(data: T2)(implicit d1: DummyImplicit, d2: DummyImplicit): Var_ = Var_(StdMatrix(data))

  }

  object Const {

    def apply(data: T0): Const_ = Const_(StdScalar(data))
    def apply(data: T1): Const_ = Const_(StdVector(data))
    def apply(data: T2)(implicit d: DummyImplicit): Const_ = Const_(StdMatrix(data))

  }

  implicit def fromByte(v: Byte):     Const_ = Const(v.toFloat)
  implicit def fromShort(v: Short):   Const_ = Const(v.toFloat)
  implicit def fromInt(v: Int):       Const_ = Const(v.toFloat)
  implicit def fromLong(v: Long):     Const_ = Const(v.toFloat)
  implicit def fromDouble(v: Double): Const_ = Const(v.toFloat)
  implicit def fromFloat(v: Float):   Const_ = Const(v)

  implicit def fromVecByte(v: Vec[Byte]):   Const_ = Const(v.map(_.toDouble))
  implicit def fromVecShort(v: Vec[Short]): Const_ = Const(v.map(_.toDouble))
  implicit def fromVecInt(v: Vec[Int]):     Const_ = Const(v.map(_.toDouble))
  implicit def fromVecLong(v: Vec[Long]):   Const_ = Const(v.map(_.toDouble))
  implicit def fromVecFloat(v: Vec[Float]): Const_ = Const(v.map(_.toDouble))
  implicit def fromVec(v: Vec[Double]):     Const_ = Const(v)

  implicit def fromMatByte(v: Mat[Byte]):   Const_ = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatShort(v: Mat[Short]): Const_ = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatInt(v: Mat[Int]):     Const_ = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatLong(v: Mat[Long]):   Const_ = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatFloat(v: Mat[Float]): Const_ = Const(v.map(_.map(_.toDouble)))
  implicit def fromMat(v: Mat[Double]):     Const_ = Const(v)

}
