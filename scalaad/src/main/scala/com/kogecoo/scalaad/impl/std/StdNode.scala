package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad._
import com.kogecoo.scalaad.graph._

import scala.language.implicitConversions


trait StdNode {

  object Var {

    def apply(data: Double): Var0 = Var0(StdScalar(data))
    def apply(data: StdVec[Double]): Var1 = Var1(StdVector(data), Shape1(data.size))
    def apply(data: StdMat[Double]): Var2 = Var2(StdMatrix(data), Shape2(data.size, data(0).size))

    // Experimental
    def arbitrary(name: String, shape: S0): ArbVar0 = ArbVar0(name, None)
    def arbitrary(name: String, shape: S1): ArbVar1 = ArbVar1(name, None, shape)
    def arbitrary(name: String, shape: S2): ArbVar2 = ArbVar2(name, None, shape)
  }

  object Const {

    def apply(data: Double): Const0 = Const0(StdScalar(data))
    def apply(data: StdVec[Double]): Const1 = Const1(StdVector(data), Shape1(data.size))
    def apply(data: StdMat[Double]): Const2 = Const2(StdMatrix(data), Shape2(data.size, data(0).size))

  }

  object Arb {
  }

  implicit def fromByte(v: Byte):     Const0 = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const0 = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const0 = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const0 = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const0 = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const0 = Const(v)

  implicit def fromStdVecByte(v: StdVec[Byte]):   Const1 = Const(v.map(_.toDouble))
  implicit def fromStdVecShort(v: StdVec[Short]): Const1 = Const(v.map(_.toDouble))
  implicit def fromStdVecInt(v: StdVec[Int]):     Const1 = Const(v.map(_.toDouble))
  implicit def fromStdVecLong(v: StdVec[Long]):   Const1 = Const(v.map(_.toDouble))
  implicit def fromStdVecFloat(v: StdVec[Float]): Const1 = Const(v.map(_.toDouble))
  implicit def fromStdVec(v: StdVec[Double]):     Const1 = Const(v)

  implicit def fromStdMatByte(v: StdMat[Byte]):   Const2 = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatShort(v: StdMat[Short]): Const2 = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatInt(v: StdMat[Int]):     Const2 = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatLong(v: StdMat[Long]):   Const2 = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMatFloat(v: StdMat[Float]): Const2 = Const(v.map(_.map(_.toDouble)))
  implicit def fromStdMat(v: StdMat[Double]):     Const2 = Const(v)

}
