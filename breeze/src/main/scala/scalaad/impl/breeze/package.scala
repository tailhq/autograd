package scalaad.impl

import _root_.breeze.linalg.{BitVector, DenseMatrix, DenseVector}


package object breeze {

  type T0 = std.T0
  type T1 = DenseVector[T0]
  type T2 = DenseMatrix[T0]
  type T3 = DenseVector[DenseMatrix[T0]]
  type T4 = DenseMatrix[DenseMatrix[T0]]

  type B0 = std.B0
  type B1 = BitVector //DenseVector[B0]
  type B2 = DenseMatrix[B0]
  type B3 = DenseVector[DenseMatrix[B0]]
  type B4 = DenseMatrix[DenseMatrix[B0]]

  type Scalar[A] = A
  type Vec[A] = DenseVector[A]
  type Mat[A] = DenseMatrix[A]
  type Tensor3[A] = DenseVector[DenseMatrix[A]]

}
