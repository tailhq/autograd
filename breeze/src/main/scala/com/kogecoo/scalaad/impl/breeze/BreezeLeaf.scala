package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.{Transpose, DenseMatrix, DenseVector}
import com.kogecoo.scalaad.{Shape2, Shape1}
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.StdScalar

import scala.language.implicitConversions


trait BreezeLeaf {

  object Var {

    def apply(data: Double): Var0              = Var0(StdScalar(data))
    def apply(data: DenseVector[Double]): Var1 = Var1(BreezeVector(data), Shape1(data.size))
    def apply(data: Transpose[DenseVector[Double]]): Var1 = Var1(BreezeVector(data.t, true), Shape1(data.t.size))
    def apply(data: DenseMatrix[Double]): Var2 = Var2(BreezeMatrix(data), Shape2(data.rows, data.cols))

  }

  object Const {

    def apply(data: Double): Const0 = Const0(StdScalar(data))
    def apply(data: DenseVector[Double]): Const1 = Const1(BreezeVector(data), Shape1(data.size))
    def apply(data: Transpose[DenseVector[Double]]): Const1 = Const1(BreezeVector(data.t, true), Shape1(data.t.size))
    def apply(data: DenseMatrix[Double]): Const2 = Const2(BreezeMatrix(data), Shape2(data.rows, data.cols))

  }

  implicit def fromByte(v: Byte):     Const0 = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const0 = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const0 = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const0 = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const0 = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const0 = Const(v)

  implicit def fromDenseVectorByte(v: DenseVector[Byte]):   Const1 = Const(v.map(_.toDouble))
  implicit def fromDenseVectorShort(v: DenseVector[Short]): Const1 = Const(v.map(_.toDouble))
  implicit def fromDenseVectorInt(v: DenseVector[Int]):     Const1 = Const(v.map(_.toDouble))
  implicit def fromDenseVectorLong(v: DenseVector[Long]):   Const1 = Const(v.map(_.toDouble))
  implicit def fromDenseVectorFloat(v: DenseVector[Float]): Const1 = Const(v.map(_.toDouble))
  implicit def fromDenseVector(v: DenseVector[Double]):     Const1 = Const(v)

  implicit def fromTransDenseVectorByte(v: Transpose[DenseVector[Byte]]):   Const1 = Const(v.inner.map(_.toDouble))
  implicit def fromTransDenseVectorShort(v: Transpose[DenseVector[Short]]): Const1 = Const(v.inner.map(_.toDouble))
  implicit def fromTransDenseVectorInt(v: Transpose[DenseVector[Int]]):     Const1 = Const(v.inner.map(_.toDouble))
  implicit def fromTransDenseVectorLong(v: Transpose[DenseVector[Long]]):   Const1 = Const(v.inner.map(_.toDouble))
  implicit def fromTransDenseVectorFloat(v: Transpose[DenseVector[Float]]): Const1 = Const(v.inner.map(_.toDouble))
  implicit def fromTransDenseVector(v: Transpose[DenseVector[Double]]):     Const1 = Const(v)

  implicit def fromDenseMatrixByte(v: DenseMatrix[Byte]):   Const2 = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixShort(v: DenseMatrix[Short]): Const2 = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixInt(v: DenseMatrix[Int]):     Const2 = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixLong(v: DenseMatrix[Long]):   Const2 = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixFloat(v: DenseMatrix[Float]): Const2 = Const(v.map(_.toDouble))
  implicit def fromDenseMatrix(v: DenseMatrix[Double]):     Const2 = Const(v)

}
