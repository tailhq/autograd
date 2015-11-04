package com.kogecoo.scalaad.impl.nd4j

import org.nd4j.linalg.api.complex.IComplexNumber
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.util.ComplexUtil


abstract class ZipAndMapFunc {

  def op(origin: IComplexNumber, other: Double): IComplexNumber

  def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber

  def op(origin: Double, other: Double): Double

}

class MaxOp extends ZipAndMapFunc {

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    if (origin.absoluteValue().doubleValue() >= other) {
      origin
    } else {
      Nd4j.createComplexNumber(other, 0)
    }
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    if (origin.absoluteValue().doubleValue() >= other.absoluteValue().doubleValue()) {
      origin
    } else {
      other
    }
  }

  override def op(origin: Double, other: Double): Double = math.max(origin, other)
}

class MinOp extends ZipAndMapFunc {

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    if (origin.absoluteValue().doubleValue() <= other) {
      origin
    } else {
      Nd4j.createComplexNumber(other, 0)
    }
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    if (origin.absoluteValue().doubleValue() <= other.absoluteValue().doubleValue()) {
      origin
    } else {
      other
    }
  }

  override def op(origin: Double, other: Double): Double = math.min(origin, other)
}

class PowOp extends ZipAndMapFunc {

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


