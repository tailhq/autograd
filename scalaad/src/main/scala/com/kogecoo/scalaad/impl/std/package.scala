package com.kogecoo.scalaad.impl


package object std {

  // Matrix is expressed by Seq like:
  //              Seq(
  // |1, 2, 3|      Seq(1, 2, 3),
  // |4, 5, 6| ->   Seq(4, 5, 6),
  // |7, 8, 9|      Seq(7, 8, 9)
  //              )

  type T0 = Double
  type T1 = Vec[T0]
  type T2 = Mat[T0]

  type Scalar[T] = T
  type Vec[T]    = Seq[T]
  type Mat[T]    = Seq[Vec[T]]

  type B0 = Boolean
  type B1 = Vec[B0]
  type B2 = Mat[B0]

  def toVec[A](s: Scalar[A]): Vec[A] = higher(s)
  def toMat[A](s: Scalar[A]): Mat[A] = higher(higher(s))
  def toMat[A](v: Vec[A]): Mat[A] = higher(v)

  def higher[A](a: A): Seq[A] = Seq(a)

}
