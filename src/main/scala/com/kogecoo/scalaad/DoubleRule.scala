package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.{Scalar, ScalarConst}
import com.kogecoo.scalaad.rule.{ MathRule, ValueRule }

import scala.language.implicitConversions

object DoubleRule {

  object Implicits {

    implicit val doubleValueRule = new DoubleValueRule

    // Literal conversion for constructing computational tree
    implicit def fromByte(v: Byte):     ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromShort(v: Short):   ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromInt(v: Int):       ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromLong(v: Long):     ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromFloat(v: Float):   ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v.toDouble)
    implicit def fromDouble(v: Double): ScalarConst[Scalar, Double] = ScalarConst[Scalar, Double](v)
  }

  class DoubleValueRule extends DoubleValueRuleBase with DoubleMathRule

  trait DoubleValueRuleBase extends ValueRule[Scalar, Double] {
    override val zeroAdd: Scalar[Double] = Scalar(0.0)
    override val zeroMul: Scalar[Double] = Scalar(1.0)
    override val derivConst: Scalar[Double] = Scalar(0.0)

    override def wrap(v: Double): Scalar[Double] = Scalar(v)

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

    override def pos(v: Scalar[Double]): Scalar[Double] = Scalar(+v.data)
    override def neg(v: Scalar[Double]): Scalar[Double] = Scalar(-v.data)
  }

  trait DoubleMathRule extends MathRule[Scalar, Double] {
    override def sin(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.sin(v.data))
    override def cos(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.cos(v.data))
    override def tan(v: Scalar[Double]): Scalar[Double] = Scalar(scala.math.tan(v.data))
    override def ln(v: Scalar[Double]):  Scalar[Double] = Scalar(scala.math.log(v.data))
  }
}
