package scalaad.impl.nd4j.op

import org.nd4j.linalg.api.complex.IComplexNumber
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.{BaseOp, Op}
import org.nd4j.linalg.factory.Nd4j


class AndOp(x: INDArray, y: INDArray, z: INDArray) extends BaseOp(x, y, z, x.length()) {

  override def name(): String = "and"

  private[this] def complex2Bool(i: IComplexNumber): Boolean = {
    i.realComponent().doubleValue() != 0.0
  }
  private[this] def and(a: Boolean, b: Boolean): Double = {
    if (a && b) 1.0 else 0.0
  }

  override def opForDimension(i: Int, i1: Int): Op = {
    val x2 = x.tensorAlongDimension(i, i1)
    val y2 = y.tensorAlongDimension(i, i1)
    val z2 = z.tensorAlongDimension(i, i1)
    new AndOp(x2, y2, z2)
  }

  override def opForDimension(i: Int, ints: Int*): Op = {
    val x2 = x.tensorAlongDimension(i, ints:_*)
    val y2 = y.tensorAlongDimension(i, ints:_*)
    val z2 = z.tensorAlongDimension(i, ints:_*)
    new AndOp(x2, y2, z2)
  }

  override def op(origin: IComplexNumber, other: Double): IComplexNumber = {
    val v = and(complex2Bool(origin), other != 0.0)
    Nd4j.createComplexNumber(v , 0)
  }

  override def op(origin: IComplexNumber, other: Float): IComplexNumber = {
    val v = and(complex2Bool(origin), other != 0.0f)
    Nd4j.createComplexNumber(v , 0)
  }

  override def op(origin: IComplexNumber, other: IComplexNumber): IComplexNumber = {
    val v = and(origin.realComponent().doubleValue() != 0.0, other.realComponent().doubleValue() != 0.0)
    Nd4j.createComplexNumber(v , 0)
  }

  override def op(origin: Float, other: Float): Float = {
    and(origin != 0.0f, other != 0.0f).toFloat
  }

  override def op(origin: Double, other: Double): Double = {
    and(origin != 0.0f, other != 0.0f)
  }

  override def op(origin: Double): Double = origin

  override def op(origin: Float): Float = origin

  override def op(origin: IComplexNumber): IComplexNumber = origin

}
