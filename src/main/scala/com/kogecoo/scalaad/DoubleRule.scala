package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.{ Scalar, ScalarConst}
import com.kogecoo.scalaad.rule._

import scala.language.implicitConversions

object DoubleRule {

  object Implicits {

    implicit val doubleRule = new DoubleRule
    implicit val doubleWrapperRule = new DoubleWrapperRule

    // Literal conversion for constructing computational tree
    implicit def fromByte(v: Byte):     ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromShort(v: Short):   ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromInt(v: Int):       ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromLong(v: Long):     ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromFloat(v: Float):   ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromDouble(v: Double): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v)
  }

  class DoubleRule extends DoubleValueRule with DoubleMathRule

  trait DoubleValueRule extends ValueRule[Scalar, Double] {
    override val zeroAdd: Value[Scalar, Double] = toValue(0.0)
    override val zeroMul: Value[Scalar, Double] = toValue(1.0)
    override val derivConst: Value[Scalar, Double] = toValue(0.0)

    override def toValue(v: Double): Value[Scalar, Double] = {
      NonContainerValue[Scalar, Double](v)
    }

    override def toValue(v: Scalar[Double])(implicit e: DummyImplicit): Value[Scalar, Double] = {
      NonContainerValue[Scalar, Double](v.data)
    }

    override def addSS(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data + r.data)
    override def subSS(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data - r.data)
    override def mulSS(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data * r.data)
    override def divSS(l: Scalar[Double], r: Scalar[Double]): Scalar[Double] = Scalar(l.data / r.data)

    override def addSM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data + r)
    override def subSM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data - r)
    override def mulSM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data * r)
    override def divSM(l: Scalar[Double], r: Double): Scalar[Double] = Scalar(l.data / r)

    override def addMS(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l + r.data)
    override def subMS(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l - r.data)
    override def mulMS(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l * r.data)
    override def divMS(l: Double, r: Scalar[Double]): Scalar[Double] = Scalar(l / r.data)

    override def addMM(l: Double, r: Double): Double = l + r
    override def subMM(l: Double, r: Double): Double = l - r
    override def mulMM(l: Double, r: Double): Double = l * r
    override def divMM(l: Double, r: Double): Double = l / r

    override def posS(v: Scalar[Double]): Scalar[Double] = Scalar(+v.data)
    override def negS(v: Scalar[Double]): Scalar[Double] = Scalar(-v.data)

    override def posM(v: Double): Double = +v
    override def negM(v: Double): Double = -v
  }

  trait DoubleMathRule extends MathRule[Scalar, Double] {

    override def sinS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.sin(v.data))
    override def cosS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.cos(v.data))
    override def tanS(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.tan(v.data))
    override def lnS(v: Scalar[Double]):  Scalar[Double] = Scalar(scala.math.log(v.data))

    override def sinM(v: Double): Double = scala.math.sin(v)
    override def cosM(v: Double): Double = scala.math.cos(v)
    override def tanM(v: Double): Double = scala.math.tan(v)
    override def lnM(v: Double):  Double = scala.math.log(v)

  }

  class DoubleWrapperRule extends ValueWrapperRule[Double, Scalar, Double] {
    override def toWrapper(src: Double): Scalar[Double] = Scalar(src)
  }

}
