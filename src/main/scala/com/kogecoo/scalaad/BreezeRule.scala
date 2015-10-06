package com.kogecoo.scalaad

import com.kogecoo.scalaad.ScalarRule.Implicits._
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.rule._

import breeze.linalg._
import scala.language.implicitConversions


object BreezeRule {

  type T = Double
  type V = DenseVector[T]
  type M = DenseMatrix[T]

  object Implicits {

    implicit val denseVectorRule = new DenseVectorRule
    implicit val denseMatrixRule = new DenseMatrixRule

    // Literal conversion for constructing computational tree
    implicit def fromByte(v: Byte):     ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)
    implicit def fromShort(v: Short):   ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)
    implicit def fromInt(v: Int):       ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)
    implicit def fromLong(v: Long):     ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)
    implicit def fromFloat(v: Float):   ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v.toDouble)
    implicit def fromDouble(v: Double): ScalarConst[DenseVector, T] = ScalarConst[DenseVector, T](v)
  }

  class DenseVectorRule extends DenseVectorValueRule with DenseVectorMathRule
  class DenseMatrixRule extends DenseMatrixValueRule with DenseMatrixMathRule

  trait DenseVectorValueRule extends ValueRule[DenseVector, T]{
    override val zeroAdd: Value[DenseVector, T]    = toValue(0.0)
    override val zeroMul: Value[DenseVector, T]    = toValue(1.0)
    override val derivConst: Value[DenseVector, T] = toValue(0.0)

    override def toValue(v: T): Value[DenseVector, T] = NonContainerValue[DenseVector, T](v)
    override def toValue(v: V)(implicit e: DummyImplicit): Value[DenseVector, T] = {
      ContainerValue[DenseVector, T](v)
    }

    override def addSS(l: V, r: V): V = l + r
    override def subSS(l: V, r: V): V = l - r
    override def mulSS(l: V, r: V): V = l :* r
    override def divSS(l: V, r: V): V = l :/ r

    override def addSM(l: V, r: T): V = l + r
    override def subSM(l: V, r: T): V = l - r
    override def mulSM(l: V, r: T): V = l * r
    override def divSM(l: V, r: T): V = l / r

    override def addMS(l: T, r: V): V = r + l
    override def subMS(l: T, r: V): V = r.map(l - _)
    override def mulMS(l: T, r: V): V = r :* l
    override def divMS(l: T, r: V): V = r.map(l / _)

    override def addMM(l: T, r: T): T = l + r
    override def subMM(l: T, r: T): T = l - r
    override def mulMM(l: T, r: T): T = l * r
    override def divMM(l: T, r: T): T = l / r

    override def posS(v: V): V = v
    override def negS(v: V): V = -v

    override def posM(v: T): T = v
    override def negM(v: T): T = -v
  }

  trait DenseVectorMathRule extends MathRule[DenseVector, T] {

    override def sinS(v: V): V = breeze.numerics.sin(v)
    override def cosS(v: V): V = breeze.numerics.cos(v)
    override def tanS(v: V): V = breeze.numerics.tan(v)
    override def asinS(v: V): V = breeze.numerics.asin(v)
    override def acosS(v: V): V = breeze.numerics.acos(v)
    override def atanS(v: V): V = breeze.numerics.atan(v)
    override def sinhS(v: V): V = breeze.numerics.sinh(v)
    override def coshS(v: V): V = breeze.numerics.cosh(v)
    override def tanhS(v: V): V = breeze.numerics.tanh(v)
    override def lnS(v: V):  V = breeze.numerics.log(v)
    override def expS(v: V):  V = breeze.numerics.exp(v)
    override def absS(v: V):  V = breeze.numerics.abs(v)
    override def sqrtS(v: V):  V = breeze.numerics.sqrt(v)

    override def sinM(v: T): T = breeze.numerics.sin(v)
    override def cosM(v: T): T = breeze.numerics.cos(v)
    override def tanM(v: T): T = breeze.numerics.tan(v)
    override def asinM(v: T): T = breeze.numerics.asin(v)
    override def acosM(v: T): T = breeze.numerics.acos(v)
    override def atanM(v: T): T = breeze.numerics.atan(v)
    override def sinhM(v: T): T = breeze.numerics.sinh(v)
    override def coshM(v: T): T = breeze.numerics.cosh(v)
    override def tanhM(v: T): T = breeze.numerics.tanh(v)
    override def lnM(v: T):  T = breeze.numerics.log(v)
    override def expM(v: T):  T = breeze.numerics.exp(v)
    override def absM(v: T):  T = breeze.numerics.abs(v)
    override def sqrtM(v: T):  T = breeze.numerics.sqrt(v)

    override def powSS(v: V, p: V): V = breeze.numerics.pow(v, p)
    override def powSM(v: V, p: T): V = breeze.numerics.pow(v, p)
    override def powMS(v: T, p: V): V = breeze.numerics.pow(v, p)
    override def powMM(v: T, p: T): T = breeze.numerics.pow(v, p)
  }

  trait DenseMatrixValueRule extends ValueRule[DenseMatrix, T] {
    override val zeroAdd: Value[DenseMatrix, T]    = toValue(0.0)
    override val zeroMul: Value[DenseMatrix, T]    = toValue(1.0)
    override val derivConst: Value[DenseMatrix, T] = toValue(0.0)

    override def toValue(v: T): Value[DenseMatrix, T] = NonContainerValue[DenseMatrix, T](v)
    override def toValue(v: M)(implicit e: DummyImplicit): Value[DenseMatrix, T] = {
      ContainerValue[DenseMatrix, T](v)
    }

    override def addSS(l: M, r: M): M = l + r
    override def subSS(l: M, r: M): M = l - r
    override def mulSS(l: M, r: M): M = l :* r
    override def divSS(l: M, r: M): M = l :/ r

    override def addSM(l: M, r: T): M = l + r
    override def subSM(l: M, r: T): M = l - r
    override def mulSM(l: M, r: T): M = l * r
    override def divSM(l: M, r: T): M = l / r

    override def addMS(l: T, r: M): M = r + l
    override def subMS(l: T, r: M): M = r.map(l - _)
    override def mulMS(l: T, r: M): M = r :* l
    override def divMS(l: T, r: M): M = r.map(l / _)

    override def addMM(l: T, r: T): T = l + r
    override def subMM(l: T, r: T): T = l - r
    override def mulMM(l: T, r: T): T = l * r
    override def divMM(l: T, r: T): T = l / r

    override def posS(v: M): M = v
    override def negS(v: M): M = -v

    override def posM(v: T): T = v
    override def negM(v: T): T = -v

  }

  trait DenseMatrixMathRule extends MathRule[DenseMatrix, T] {

    override def sinS(v: M): M = breeze.numerics.sin(v)
    override def cosS(v: M): M = breeze.numerics.cos(v)
    override def tanS(v: M): M = breeze.numerics.tan(v)
    override def asinS(v: M): M = breeze.numerics.asin(v)
    override def acosS(v: M): M = breeze.numerics.acos(v)
    override def atanS(v: M): M = breeze.numerics.atan(v)
    override def sinhS(v: M): M = breeze.numerics.sinh(v)
    override def coshS(v: M): M = breeze.numerics.cosh(v)
    override def tanhS(v: M): M = breeze.numerics.tanh(v)
    override def lnS(v: M):  M = breeze.numerics.log(v)
    override def expS(v: M):  M = breeze.numerics.exp(v)
    override def absS(v: M):  M = breeze.numerics.abs(v)
    override def sqrtS(v: M):  M = breeze.numerics.sqrt(v)

    override def sinM(v: T): T = breeze.numerics.sin(v)
    override def cosM(v: T): T = breeze.numerics.cos(v)
    override def tanM(v: T): T = breeze.numerics.tan(v)
    override def asinM(v: T): T = breeze.numerics.asin(v)
    override def acosM(v: T): T = breeze.numerics.acos(v)
    override def atanM(v: T): T = breeze.numerics.atan(v)
    override def sinhM(v: T): T = breeze.numerics.sinh(v)
    override def coshM(v: T): T = breeze.numerics.cosh(v)
    override def tanhM(v: T): T = breeze.numerics.tanh(v)
    override def lnM(v: T):  T = breeze.numerics.log(v)
    override def expM(v: T):  T = breeze.numerics.exp(v)
    override def absM(v: T):  T = breeze.numerics.abs(v)
    override def sqrtM(v: T):  T = breeze.numerics.sqrt(v)

    override def powSS(v: M, p: M): M = breeze.numerics.pow(v, p)
    override def powSM(v: M, p: T): M = breeze.numerics.pow(v, p)
    override def powMS(v: T, p: M): M = breeze.numerics.pow(v, p)
    override def powMM(v: T, p: T): T = breeze.numerics.pow(v, p)

  }

}
