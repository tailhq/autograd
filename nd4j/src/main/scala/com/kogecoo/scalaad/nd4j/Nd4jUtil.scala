package com.kogecoo.scalaad.nd4j

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.impl.scalar.comparison.{ScalarGreaterThanOrEqual, ScalarLessThanOrEqual}
import org.nd4j.linalg.api.ops.impl.transforms.comparison.{GreaterThanOrEqual, LessThanOrEqual}
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._


object Nd4jUtil {

  def where(cond: WhereCondition, x: INDArray, y: INDArray): INDArray = {
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

