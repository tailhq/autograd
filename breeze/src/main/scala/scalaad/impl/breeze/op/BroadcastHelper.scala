package scalaad.impl.breeze.op

import breeze.linalg.{BitVector, DenseMatrix, DenseVector}

import scala.reflect.ClassTag
import scalaad.impl.breeze.{B0, B1, Mat, T0, T1, Tensor3, Vec}


object BroadcastHelper {

  def perform_T1_T1_T1(a: T1, b: T1, f10: (T1, T0) => T1, f11: (T1, T1) => T1, exchangable: Boolean): T1 = {
    (a.length, b.length) match {
      case (x, y) if x * y != 0 && x == y  => f11(a, b)
      case (x, 1) if x != 0                => f10(a, b(0))
      case (1, y) if y != 0 && exchangable => f10(b, a(0))
      case (1, y) if y != 0                => f11(BroadcastHelper.vecFillLike(a(0), b), b)
      case _                               => throw new Exception(s"cannot perform broadcasting op with $a and $b")
    }
  }

  def perform_T1_T1_B1(a: T1, b: T1, f10: (T1, T0) => B1, f11: (T1, T1) => B1, exchangable: Boolean): B1 = {
    (a.length, b.length) match {
      case (x, y) if x * y != 0 && x == y  => f11(a, b)
      case (x, 1) if x != 0                => f10(a, b(0))
      case (1, y) if y != 0 && exchangable => f10(b, a(0))
      case (1, y) if y != 0                => f11(vecFillLike(a(0), b), b)
      case _                               => throw new Exception(s"cannot perform broadcasting op with $a and $b")
    }
  }

  def perform_B1_B1_B1(a: B1, b: B1, f10: (B1, B0) => B1, f11: (B1, B1) => B1, exchangable: Boolean): B1 = {
    (a.length, b.length) match {
      case (x, y) if x * y != 0 && x == y  => f11(a, b)
      case (x, 1) if x != 0                => f10(a, b(0))
      case (1, y) if y != 0 && exchangable => f10(b, a(0))
      case (1, y) if y != 0                => f11(vecFillLike(a(0), b), b)
      case _                               => throw new Exception(s"cannot perform broadcasting op with $a and $b")
    }
  }

  def perform_2_2_2[A0: ClassTag:breeze.storage.Zero, R0](a: Mat[A0], b: Mat[A0], f20: (Mat[A0], A0) => Mat[R0], f22: (Mat[A0], Mat[A0]) => Mat[R0], exchangable: Boolean): Mat[R0] = {
    (a.rows, b.rows) match {
      case (x, y) if x * y != 0 && x == y => (a.cols, b.cols) match {
        case (z, w) if z * w != 0 && z == w => f22(a, b)
        case (z, 1) if z != 0               => f22(a, repeatHorz(b(::, 0), z))
        case (1, w) if w != 0               => f22(repeatHorz(a(::, 0), w), b)
        case _                              => throw new Exception(s"cannot perform broadcasting op with $a and $b")
      }

      case (x, 1) if x != 0 => (a.cols, b.cols) match {
        case (z, w) if z * w != 0 && z == w => f22(a, repeatVert(b(0, ::).inner, x))
        case (z, 1) if z != 0               => f20(a, b((0, 0)))
        case (1, w) if w != 0               => f22(repeatHorz(a(::, 0), w), repeatVert(b(0, ::).inner, x))
        case _                              => throw new Exception(s"cannot perform broadcasting op with $a and $b")
      }

      case (1, y) if y != 0 => (a.cols, b.cols) match {
        case (z, w) if z * w != 0 && z == w  => f22(repeatVert(a(0, ::).inner, y), b)
        case (z, 1) if z != 0                => f22(repeatVert(a(0, ::).inner, y), repeatHorz(b(::, 0), z))
        case (1, w) if w != 0 && exchangable => f20(b, a((0, 0)))
        case (1, w) if w != 0                => f22(matFillLike(a(0, 0), b), b)
        case _                               => throw new Exception(s"cannot perform broadcasting op with $a and $b")
      }
      case _                              => throw new Exception(s"cannot perform broadcasting op with $a and $b")
    }
  }

  def perform_3_3_3[A0: ClassTag:breeze.storage.Zero, R0](a: Tensor3[A0], b: Tensor3[A0], f20: (Mat[A0], A0) => Mat[R0], f22: (Mat[A0], Mat[A0]) => Mat[R0], exchangable: Boolean): Tensor3[R0] = {
    (a.length, b.length) match {
      case (x, y) if x * y != 0 && x == y => a.mapPairs{ case (k, v) => f22(v, b(k)) }
      case (x, 1) if x != 0               => a.map(perform_2_2_2(_, b(0), f20, f22, exchangable))
      case (1, y) if y != 0               => b.map(perform_2_2_2(a(0), _, f20, f22, exchangable))
      case _                              => throw new Exception(s"cannot perform broadcasting op with $a and $b")
    }
  }

  def vecFillLike(v: B0, like: B1): B1 = BitVector(Array.fill(like.length)(v): _*)

  def vecFillLike(v: T0, like: T1): T1 = DenseVector.fill[T0](like.length)(v)

  def matFillLike[A0: ClassTag:breeze.storage.Zero](v: A0, like: Mat[A0]): Mat[A0] = DenseMatrix.fill[A0](like.rows, like.cols)(v)

  def repeatVert[A0: ClassTag:breeze.storage.Zero](v: Vec[A0], repeat: Int): Mat[A0] = {
    val x = v.toDenseMatrix
    (1 until repeat).foldLeft(x){ case (m, _) => DenseMatrix.vertcat(m, x) }
  }

  def repeatHorz[A0: ClassTag:breeze.storage.Zero](v: Vec[A0], repeat: Int): Mat[A0] = {
    val x = v.toDenseMatrix.t
    (1 until repeat).foldLeft(x){ case (m, _) => DenseMatrix.horzcat(m, x) }
  }

}
