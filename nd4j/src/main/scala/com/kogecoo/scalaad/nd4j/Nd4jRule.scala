package com.kogecoo.scalaad.nd4j

import com.kogecoo.scalaad.graph.{ContainerConst, ScalarConst}
import com.kogecoo.scalaad.rule._

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._

import scala.language.implicitConversions


object Nd4jRule {

  type T = Double
  type C = INDArray_[T]

  object Implicits {

    implicit val iNDArrayRuleRule = new INDArrayRule
    implicit val iNDArrayWrapperRule = new INDArrayWrapperRule

    // Literal conversion for constructing computational tree
    implicit def fromByte(v: Byte):     ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromShort(v: Short):   ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromInt(v: Int):       ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromLong(v: Long):     ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromFloat(v: Float):   ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v.toDouble)
    implicit def fromDouble(v: Double): ScalarConst[INDArray_, Double] = ScalarConst[INDArray_, Double](v)

    implicit def fromINDArray(v: INDArray): ContainerConst[INDArray_, Double] = {
      ContainerConst[INDArray_, Double](INDArray_(v))
    }
  }

  // wrapper for INDArray
  case class INDArray_[A](data: INDArray) { type TypeOfScalarOp = A }

  class INDArrayRule extends INDArrayValueRule with INDArrayMathRule

  trait INDArrayValueRule extends ValueRule[INDArray_, T] {

    override def zeroM: T = 0.0
    override def zeroS(shape: C): C = INDArray_[T](Nd4j.zeros(shape.data.shape():_*))
    override def oneM: T = 1.0
    override def oneS(shape: C): C = INDArray_[T](Nd4j.ones(shape.data.shape():_*))

    override def toValue(v: T): Value[INDArray_, T] = NonContainerValue[INDArray_, T](v)
    override def toValue(v: C)(implicit e: DummyImplicit): Value[INDArray_, T] = {
      ContainerValue[INDArray_, T](v)
    }

    override def addSS(l: C, r: C): C = INDArray_(l.data.add(r.data))
    override def subSS(l: C, r: C): C = INDArray_(l.data.sub(r.data))
    override def mulSS(l: C, r: C): C = INDArray_(l.data.mul(r.data))
    override def divSS(l: C, r: C): C = INDArray_(l.data.div(r.data))

    override def addSM(l: C, r: T): C = INDArray_(l.data.add(r))
    override def subSM(l: C, r: T): C = INDArray_(l.data.sub(r))
    override def mulSM(l: C, r: T): C = INDArray_(l.data.mul(r))
    override def divSM(l: C, r: T): C = INDArray_(l.data.div(r))

    override def addMS(l: T, r: C): C = INDArray_(r.data.add(l)) // it seems to be no radd in Nd4j
    override def subMS(l: T, r: C): C = INDArray_(r.data.rsub(l))
    override def mulMS(l: T, r: C): C = INDArray_(r.data.mul(l)) // it seems to be no rmul in Nd4j
    override def divMS(l: T, r: C): C = INDArray_(r.data.rdiv(l))

    override def addMM(l: T, r: T): T = l + r
    override def subMM(l: T, r: T): T = l - r
    override def mulMM(l: T, r: T): T = l * r
    override def divMM(l: T, r: T): T = l / r

    override def ltSS (l: C, r: C): INDArray_[Boolean] = INDArray_[Boolean](l.data.lt (r.data))
    override def lteSS(l: C, r: C): INDArray_[Boolean] = INDArray_[Boolean](Nd4jUtil.lte(l.data, r.data))
    override def gtSS (l: C, r: C): INDArray_[Boolean] = INDArray_[Boolean](l.data.gt (r.data))
    override def gteSS(l: C, r: C): INDArray_[Boolean] = INDArray_[Boolean](Nd4jUtil.gte(l.data, r.data))
    override def eqSS (l: C, r: C): INDArray_[Boolean] = INDArray_[Boolean](l.data.eq (r.data))

    override def ltSM (l: C, r: T): INDArray_[Boolean] = INDArray_[Boolean](l.data.lt (r))
    override def lteSM(l: C, r: T): INDArray_[Boolean] = INDArray_[Boolean](Nd4jUtil.lte(l.data, r))
    override def gtSM (l: C, r: T): INDArray_[Boolean] = INDArray_[Boolean](l.data.gt (r))
    override def gteSM(l: C, r: T): INDArray_[Boolean] = INDArray_[Boolean](Nd4jUtil.gte(l.data, r))
    override def eqSM (l: C, r: T): INDArray_[Boolean] = INDArray_[Boolean](l.data.eq (r))

    override def ltMS (l: T, r: C): INDArray_[Boolean] = INDArray_[Boolean](r.data.gt (l))
    override def lteMS(l: T, r: C): INDArray_[Boolean] = INDArray_[Boolean](Nd4jUtil.gte(r.data, l))
    override def gtMS (l: T, r: C): INDArray_[Boolean] = INDArray_[Boolean](r.data.lt (l))
    override def gteMS(l: T, r: C): INDArray_[Boolean] = INDArray_[Boolean](Nd4jUtil.lte(r.data, l))
    override def eqMS (l: T, r: C): INDArray_[Boolean] = INDArray_[Boolean](r.data.eq (l))

    override def ltMM (l: T, r: T): Boolean = l < r
    override def lteMM(l: T, r: T): Boolean = l <= r
    override def gtMM (l: T, r: T): Boolean = l > r
    override def gteMM(l: T, r: T): Boolean = l >= r
    override def eqMM (l: T, r: T): Boolean = l == r

    override def posS(v: C): C = INDArray_(v.data)
    override def negS(v: C): C = INDArray_(v.data.neg())

    override def posM(v: T): T = +v
    override def negM(v: T): T = -v

    override def transposeS(v: C): C = INDArray_(v.data.transpose())
    override def transposeM(v: T): T = v

    // FIXME: maybe we need to implement Custom executioner which suports 3 args for
    //  remove folloing (UNEFFICIENT) trick.
    override def whereSSS(cond: INDArray_[Boolean], a: C, b: C): C = INDArray_(Nd4jUtil.where(cond.data, a.data, b.data))
    override def whereSSM(cond: INDArray_[Boolean], a: C, b: T): C = INDArray_(Nd4jUtil.where(cond.data, a.data, b))
    override def whereSMS(cond: INDArray_[Boolean], a: T, b: C): C = INDArray_(Nd4jUtil.where(cond.data, a, b.data))
    override def whereSMM(cond: INDArray_[Boolean], a: T, b: T): C = INDArray_(cond.data.map(c => if (c != 0.0) a else b))
    override def whereMSS(cond: Boolean,            a: C, b: C): C = if (cond) a else b
    override def whereMSM(cond: Boolean,            a: C, b: T): C = if (cond) a else INDArray_(a.data.map(_ => b))
    override def whereMMS(cond: Boolean,            a: T, b: C): C = if (cond) INDArray_(b.data.map(_ => a)) else b
    override def whereMMM(cond: Boolean,            a: T, b: T): T = if (cond) a else b

  }

  trait INDArrayMathRule extends MathRule[INDArray_, Double] {

    override def sinS(v: C): C = INDArray_(v.data.map(scala.math.sin))
    override def cosS(v: C): C = INDArray_(v.data.map(scala.math.cos))
    override def tanS(v: C): C = INDArray_(v.data.map(scala.math.tan))
    override def asinS(v: C): C = INDArray_(v.data.map(scala.math.asin))
    override def acosS(v: C): C = INDArray_(v.data.map(scala.math.acos))
    override def atanS(v: C): C = INDArray_(v.data.map(scala.math.atan))
    override def sinhS(v: C): C = INDArray_(v.data.map(scala.math.sinh))
    override def coshS(v: C): C = INDArray_(v.data.map(scala.math.cosh))
    override def tanhS(v: C): C = INDArray_(v.data.map(scala.math.tanh))
    override def lnS(v: C):  C = INDArray_(v.data.map(scala.math.log))
    override def expS(v: C):  C = INDArray_(v.data.map(scala.math.exp))
    override def absS(v: C):  C = INDArray_(v.data.map(scala.math.abs))
    override def sqrtS(v: C):  C = INDArray_(v.data.map(scala.math.sqrt))

    override def sinM(v: T): T = scala.math.sin(v)
    override def cosM(v: T): T = scala.math.cos(v)
    override def tanM(v: T): T = scala.math.tan(v)
    override def asinM(v: T): T = scala.math.asin(v)
    override def acosM(v: T): T = scala.math.acos(v)
    override def atanM(v: T): T = scala.math.atan(v)
    override def sinhM(v: T): T = scala.math.sinh(v)
    override def coshM(v: T): T = scala.math.cosh(v)
    override def tanhM(v: T): T = scala.math.tanh(v)
    override def lnM(v: T):  T = scala.math.log(v)
    override def expM(v: T):  T = scala.math.exp(v)
    override def absM(v: T):  T = scala.math.abs(v)
    override def sqrtM(v: T):  T = scala.math.sqrt(v)

    override def powSS(v: C, p: C): C = INDArray_(Nd4jUtil.zipAndMap(new Pow, v.data, p.data))
    override def powSM(v: C, p: T): C = INDArray_(v.data.map(scala.math.pow(_, p)))
    override def powMS(v: T, p: C): C = INDArray_(p.data.map(scala.math.pow(v, _)))
    override def powMM(v: T, p: T): T = scala.math.pow(v, p)

    override def dotSS(a: C, b: C): C = INDArray_(a.data.dot(b.data))
    override def dotSM(a: C, b: T): C = INDArray_(a.data * b)
    override def dotMS(a: T, b: C): C = INDArray_(b.data * a)
    override def dotMM(a: T, b: T): T = a * b

  }

  class INDArrayWrapperRule extends ValueWrapperRule[INDArray, INDArray_, Double] {
    override def toWrapper(src: INDArray): INDArray_[Double] = INDArray_(src)
  }

}



