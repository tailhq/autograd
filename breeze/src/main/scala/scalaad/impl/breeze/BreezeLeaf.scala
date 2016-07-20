package scalaad.impl.breeze

import breeze.linalg.{DenseMatrix, DenseVector}

import scala.language.implicitConversions
import scalaad.graph.{Real, Const => Const_, Var => Var_}
import scalaad.impl.std.StdScalar


trait BreezeLeaf {

  object Var {

    def apply(data: Double): Var_              = Var_(StdScalar(data))
    def apply(data: DenseVector[Double]): Var_ = Var_(BreezeVector(data))
    def apply(data: DenseMatrix[Double]): Var_ = Var_(BreezeMatrix(data))

  }

  object Const {

    def apply(data: Double): Const_[Real] = Const_(StdScalar(data))
    def apply(data: DenseVector[Double]): Const_[Real] = Const_(BreezeVector(data))
    def apply(data: DenseMatrix[Double]): Const_[Real] = Const_(BreezeMatrix(data))

  }

  implicit def fromByte(v: Byte):     Const_[Real] = Const(v.toDouble)
  implicit def fromShort(v: Short):   Const_[Real] = Const(v.toDouble)
  implicit def fromInt(v: Int):       Const_[Real] = Const(v.toDouble)
  implicit def fromLong(v: Long):     Const_[Real] = Const(v.toDouble)
  implicit def fromFloat(v: Float):   Const_[Real] = Const(v.toDouble)
  implicit def fromDouble(v: Double): Const_[Real] = Const(v)

  implicit def fromDenseVectorByte(v: DenseVector[Byte]):   Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseVectorShort(v: DenseVector[Short]): Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseVectorInt(v: DenseVector[Int]):     Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseVectorLong(v: DenseVector[Long]):   Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseVectorFloat(v: DenseVector[Float]): Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseVector(v: DenseVector[Double]):     Const_[Real] = Const(v)

  implicit def fromDenseMatrixByte(v: DenseMatrix[Byte]):   Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixShort(v: DenseMatrix[Short]): Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixInt(v: DenseMatrix[Int]):     Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixLong(v: DenseMatrix[Long]):   Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseMatrixFloat(v: DenseMatrix[Float]): Const_[Real] = Const(v.map(_.toDouble))
  implicit def fromDenseMatrix(v: DenseMatrix[Double]):     Const_[Real] = Const(v)

}
