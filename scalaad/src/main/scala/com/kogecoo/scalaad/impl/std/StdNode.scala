package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad._
import com.kogecoo.scalaad.graph.{ArbVar0, ArbVar1, ArbVar2, Var => Var_, Const => Const_}
import shapeless.Nat.{_0, _1, _2}

import scala.language.implicitConversions


trait StdNode {

  object Var {

    def apply(data: T0): Var_[_0] = Var_[_0](StdScalar(data))
    def apply(data: T1)(implicit d: DummyImplicit): Var_[_1] = Var_[_1](StdVector(data))
    def apply(data: T2)(implicit d1: DummyImplicit, d2: DummyImplicit): Var_[_2] = Var_[_2](StdMatrix(data))

    // Experimental
    def arbitrary(name: String, shape: Shape[_0]): ArbVar0 = ArbVar0(name)
    def arbitrary(name: String, shape: Shape[_1])(implicit d: DummyImplicit): ArbVar1 = ArbVar1(name, shape)
    def arbitrary(name: String, shape: Shape[_2])(implicit d1: DummyImplicit, d2: DummyImplicit): ArbVar2 = ArbVar2(name, shape)
  }

  object Const {

    def apply(data: T0): Const_[_0] = Const_[_0](StdScalar(data))
    def apply(data: T1): Const_[_1] = Const_[_1](StdVector(data))
    def apply(data: T2)(implicit d: DummyImplicit): Const_[_2] = Const_[_2](StdMatrix(data))

  }

  object Arb {
  }

  implicit def fromByte(v: Byte):     Const_[_0] = Const(v.toFloat)
  implicit def fromShort(v: Short):   Const_[_0] = Const(v.toFloat)
  implicit def fromInt(v: Int):       Const_[_0] = Const(v.toFloat)
  implicit def fromLong(v: Long):     Const_[_0] = Const(v.toFloat)
  implicit def fromDouble(v: Double): Const_[_0] = Const(v.toFloat)
  implicit def fromFloat(v: Float):   Const_[_0] = Const(v)

  implicit def fromVecByte(v: Vec[Byte]):   Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromVecShort(v: Vec[Short]): Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromVecInt(v: Vec[Int]):     Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromVecLong(v: Vec[Long]):   Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromVecFloat(v: Vec[Float]): Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromVec(v: Vec[Double]):     Const_[_1] = Const(v)

  implicit def fromMatByte(v: Mat[Byte]):   Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatShort(v: Mat[Short]): Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatInt(v: Mat[Int]):     Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatLong(v: Mat[Long]):   Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMatFloat(v: Mat[Float]): Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromMat(v: Mat[Double]):     Const_[_2] = Const(v)

}
