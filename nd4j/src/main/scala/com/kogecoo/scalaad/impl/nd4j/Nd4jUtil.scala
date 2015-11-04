package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.impl.nd4j.op._
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.impl.accum.Dot
import org.nd4j.linalg.api.ops.impl.scalar.comparison._
import org.nd4j.linalg.api.ops.impl.transforms.comparison._
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._


object Nd4jUtil {

  def toBool(x: Double): Boolean = x != 0.0

  def where(cond: WhereCondition, x: INDArray, y: INDArray): INDArray = {
    val dup = x.dup()
    Nd4j.getExecutioner.exec(
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
    Nd4j.getExecutioner.exec(
      new ZipAndMapOp(f, dup.linearView(), y.linearView(), dup.linearView())
    )
    dup
  }

  def max(self: INDArray, other: INDArray): INDArray = zipAndMap(new MaxOp, self, other)

  def max(self: INDArray, other: Double): INDArray = self.map(math.max(_, other))

  def min(self: INDArray, other: INDArray): INDArray = zipAndMap(new MinOp, self, other)

  def min(self: INDArray, other: Double): INDArray = self.map(math.min(_, other))

  def pow(self: INDArray, other: INDArray): INDArray = zipAndMap(new PowOp, self, other)

  def pow(self: INDArray, other: Double): INDArray = self.map(math.pow(_, other))

  def pow(self: Double, other: INDArray): INDArray = other.map(math.pow(self, _))

  def dot(self: INDArray, other: INDArray): Double = {
    val dup = self.dup()
    Nd4j.getExecutioner.execAndReturn(
      new Dot(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    ).currentResult().doubleValue() // FIXME
  }

  def eq(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new EqualTo(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def eq(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new ScalarEquals(dup.linearView(), other)
    )
    dup
  }

  def neq(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new NotEqualTo(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def neq(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new ScalarNotEquals(dup.linearView(), other)
    )
    dup
  }

  def lt(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new LessThan(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def lt(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new ScalarLessThan(dup.linearView(), other)
    )
    dup
  }

  def lte(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new LessThanOrEqual(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def lte(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new ScalarLessThanOrEqual(dup.linearView(), other)
    )
    dup
  }

  def gt(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new GreaterThan(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def gt(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new ScalarGreaterThan(dup.linearView(), other)
    )
    dup
  }

  def gte(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new GreaterThanOrEqual(dup.linearView(), other.linearView(), dup.linearView(), dup.length())
    )
    dup
  }

  def gte(self: INDArray, other: Double): INDArray = {
    val dup = self.dup()
    Nd4j.getExecutioner.exec(
      new ScalarGreaterThanOrEqual(dup.linearView(), other)
    )
    dup
  }

  def and(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
     Nd4j.getExecutioner.exec(
      new AndOp(dup.linearView(), other.linearView(), dup.linearView())
    )
    dup
  }

  def and(self: INDArray, other: Boolean): INDArray = {
    self.map { a => if (toBool(a) && other) 1.0 else 0.0 }
  }

  def or(self: INDArray, other: INDArray): INDArray = {
    val dup = self.dup()
     Nd4j.getExecutioner.exec(
      new OrOp(dup.linearView(), other.linearView(), dup.linearView())
    )
    dup
  }

  def or(self: INDArray, other: Boolean): INDArray = {
    self.map { a => if (toBool(a) && other) 1.0 else 0.0 }
  }

}
