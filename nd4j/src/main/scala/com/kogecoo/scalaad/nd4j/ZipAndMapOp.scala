package com.kogecoo.scalaad.nd4j

import org.nd4j.linalg.api.complex.IComplexNumber
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.{BaseOp, Op}


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
