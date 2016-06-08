package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad._
import com.kogecoo.scalaad.graph.{ArbVar0, ArbVar1, ArbVar2, Const => Const_, Var0, Var1, Var2}
import shapeless.Nat.{_0, _1, _2}

import scala.language.implicitConversions


trait StdNode {

  object Var {

    def apply(data: Double): Var0 = Var0(StdScalar(data))
    def apply(data: StdVec[Double]): Var1 = Var1(StdVector(data))
    def apply(data: StdMat[Double]): Var2 = Var2(StdMatrix(data))

    // Experimental
    def arbitrary(name: String, shape: Shape[_0]): ArbVar0 = ArbVar0(name)
    def arbitrary(name: String, shape: Shape[_1]): ArbVar1 = ArbVar1(name, shape)
    def arbitrary(name: String, shape: Shape[_2]): ArbVar2 = ArbVar2(name, shape)
  }

  object Const {

    def apply(data: Double): Const_[_0] = Const_[_0](StdScalar(data))
    def apply(data: StdVec[Double]): Const_[_1] = Const_[_1](StdVector(data))
    def apply(data: StdMat[Double]): Const_[_2] = Const_[_2](StdMatrix(data))

  }

  object Arb {
  }

  implicit def fromByte(v: Byte):     Const_[_0] = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const_[_0] = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const_[_0] = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const_[_0] = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const_[_0] = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const_[_0] = Const(v)

  implicit def fromStdVecByte(v: StdVec[Byte]):   Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromStdVecShort(v: StdVec[Short]): Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromStdVecInt(v: StdVec[Int]):     Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromStdVecLong(v: StdVec[Long]):   Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromStdVecFloat(v: StdVec[Float]): Const_[_1] = Const(v.map(_.toDouble))
  implicit def fromStdVec(v: StdVec[Double]):     Const_[_1] = Const(v)

  implicit def fromStdMatByte(v: StdMat[Byte]):   Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatShort(v: StdMat[Short]): Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatInt(v: StdMat[Int]):     Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatLong(v: StdMat[Long]):   Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatFloat(v: StdMat[Float]): Const_[_2] = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMat(v: StdMat[Double]):     Const_[_2] = Const(v)

}
