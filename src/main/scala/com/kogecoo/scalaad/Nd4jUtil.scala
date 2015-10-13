package com.kogecoo.scalaad

import org.nd4j.linalg.api.complex.IComplexNumber
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.impl.scalar.comparison.{ScalarGreaterThanOrEqual, ScalarLessThanOrEqual}
import org.nd4j.linalg.api.ops.impl.transforms.comparison.{GreaterThanOrEqual, LessThanOrEqual}
import org.nd4j.linalg.api.ops.{Op, BaseOp}
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.util.ComplexUtil
import org.nd4s.Implicits._


class ZipAndMapOp[A <: ZipAndMapFunc](f: A, x: INDArray, y: INDArray, z: INDArray) extends BaseOp(x, y, z, x.length()) {

  override def name(): String = "zip_and_map"

  override def opForDimension(i: Int, i1: Int): Op = {
    val x2 = x.tensorAlongDimension(i, i1)
    val y2 = y.tensorAlongDimension(i, i1)
    val z2 = z.tensorAlongDimension(i, i1)
    new ZipAndMapOp(f, x2, y2, z2)
  }

  override def opForDimension(i: Int, ints: Int*): Op = {
    val x2 = x.tensorAlongDimension(i, ints:_*)
    val y2 = y.tensorAlongDimension(i, ints:_*)
    val z2 = z.tensorAlongDimension(i, ints:_*)
    new ZipAndMapOp(f, x2, y2, z2)
  }

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = f.op(origin, other)

  override def op(origin: IComplexNumber, other: Float): IComplexNumber = f.op(origin, other)

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = f.op(origin, other)

  override def op(origin: Float, other: Float): Float = f.op(origin.toDouble, other.toDouble).toFloat

  override def op(origin: Double, other: Double): Double = f.op(origin, other)

  override def op(origin: Double): Double = origin

  override def op(origin: Float): Float = origin

  override def op(origin: IComplexNumber): IComplexNumber = origin

}

abstract class ZipAndMapFunc {

  def op(origin: IComplexNumber, other: Double): IComplexNumber

  def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber

  def op(origin: Double, other: Double): Double

}

class Pow extends ZipAndMapFunc {

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    ComplexUtil.pow(origin, other)
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    ComplexUtil.pow(origin, other)
  }

  override def op(origin: Double, other: Double): Double = {
    scala.math.pow(origin, other)
  }
}

class EmitIfTrue extends ZipAndMapFunc {

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    if (origin.realComponent().doubleValue() != 0.0) {
      Nd4j.createComplexNumber(other, 0)
    } else {
      Nd4j.createComplexNumber(0, 0)
    }
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    if (origin.realComponent().doubleValue() != 0.0) {
      other
    } else {
      Nd4j.createComplexNumber(0, 0)
    }
  }

  override def op(origin: Double, other: Double): Double = {
    if (origin != 0.0) {
      other
    } else {
      0.0
    }
  }
}

class EmitIfFalse extends ZipAndMapFunc {

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    if (origin.realComponent().doubleValue() == 0.0) {
      Nd4j.createComplexNumber(other, 0)
    } else {
      Nd4j.createComplexNumber(0, 0)
    }
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    if (origin.realComponent().doubleValue() == 0.0) {
      other
    } else {
      Nd4j.createComplexNumber(0, 0)
    }
  }

  override def op(origin: Double, other: Double): Double = {
    if (origin == 0.0) {
      other
    } else {
      0.0
    }
  }

}

class WhereOp(cond: WhereCond, x: INDArray, y: INDArray, z: INDArray) extends BaseOp(x, y, z, x.length()) {

  override def name(): String = "where"

  override def opForDimension(i: Int, i1: Int): Op = {
    val x2 = x.tensorAlongDimension(i, i1)
    val y2 = y.tensorAlongDimension(i, i1)
    val z2 = z.tensorAlongDimension(i, i1)
    new WhereOp(cond, x2, y2, z2)
  }

  override def opForDimension(i: Int, ints: Int*): Op = {
    val x2 = x.tensorAlongDimension(i, ints:_*)
    val y2 = y.tensorAlongDimension(i, ints:_*)
    val z2 = z.tensorAlongDimension(i, ints:_*)
    new WhereOp(cond, x2, y2, z2)
  }

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    if (cond.op(origin, other)) origin else Nd4j.createComplexNumber(other, 0)
  }

  override def op(origin: IComplexNumber, other: Float): IComplexNumber = {
    if (cond.op(origin, other)) origin else Nd4j.createComplexNumber(other, 0)
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    if (cond.op(origin, other)) origin else other
  }

  override def op(origin: Float, other: Float): Float = {
    if (cond.op(origin, other)) origin else other
  }

  override def op(origin: Double, other: Double): Double = {
    if (cond.op(origin, other)) origin else other
  }

  override def op(origin: Double): Double = origin

  override def op(origin: Float): Float = origin

  override def op(origin: IComplexNumber): IComplexNumber = origin

}

abstract class WhereCond {

  def op(x: IComplexNumber, y: Double): Boolean

  def op(x: IComplexNumber, y: Float): Boolean

  def op(x: IComplexNumber, y: IComplexNumber): Boolean

  def op(x: Float, y: Float): Boolean

  def op(x: Double, y: Double): Boolean

  def op(x: Double): Boolean

  def op(x: Float): Boolean

  def op(x: IComplexNumber): Boolean

}


object INDArrayExtensions {

  def where(cond: WhereCond, x: INDArray, y: INDArray): INDArray = {
    val dup = x.dup()
    Nd4j.getExecutioner().exec(
      new WhereOp(cond, dup.linearView(), y.linearView(), dup.linearView())
    )
    dup
  }

  def where(cond: INDArray, x: INDArray, y: INDArray): INDArray = {
    val a_ = zipAndMap(new EmitIfTrue, cond, x)
    val b_ = zipAndMap(new EmitIfFalse, cond, y)
    a_.add(b_)
  }

  def where(cond: INDArray, x: INDArray, y: Double): INDArray = {
    val a_ = zipAndMap(new EmitIfTrue, cond, x)
    val b_ = cond.map(c => if (c == 0.0) y else 0.0)
    a_.add(b_)
  }

  def where(cond: INDArray, x: Double, y: INDArray): INDArray = {
    val a_ = cond.map(c => if (c != 0.0) x else 0.0)
    val b_ = zipAndMap(new EmitIfFalse, cond, y)
    a_.add(b_)
  }

  def zipAndMap(f: ZipAndMapFunc, x: INDArray, y: INDArray): INDArray = {
    val dup = x.dup()
    Nd4j.getExecutioner().exec(
      new ZipAndMapOp(f, dup.linearView(), y.linearView(), dup.linearView())
    )
    dup
  }

  def lte(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner().exec(
      new LessThanOrEqual(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def lte(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner().exec(
      new ScalarLessThanOrEqual(dup.linearView(), other)
    )
    dup
  }

  def gte(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner().exec(
      new GreaterThanOrEqual(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def gte(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner().exec(
      new ScalarGreaterThanOrEqual(dup.linearView(), other)
    )
    dup
  }

}

