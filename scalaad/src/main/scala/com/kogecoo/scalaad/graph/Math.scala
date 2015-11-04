package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape0, Shape1, Shape2}


case class Sin0(v: N0) extends Op0

case class Cos0(v: N0) extends Op0

case class Tan0(v: N0) extends Op0

case class Asin0(v: N0) extends Op0

case class Acos0(v: N0) extends Op0

case class Atan0(v: N0) extends Op0

case class Sinh0(v: N0) extends Op0

case class Cosh0(v: N0) extends Op0

case class Tanh0(v: N0) extends Op0

case class Ln0(v: N0) extends Op0

case class Exp0(v: N0) extends Op0

case class Sqrt0(v: N0) extends Op0

case class Pow00(l: N0, r: N0) extends Op00

case class Abs0(v: N0) extends Op0

case class Max00(l: N0, r: N0) extends Op00

case class Min00(l: N0, r: N0) extends Op00


// Experimental

// Norm
case class L0Norm(v: N1) extends UnaryOp[S0, S1] { val shape: S0 = Shape0() }
case class L1Norm(v: N1) extends UnaryOp[S0, S1] { val shape: S0 = Shape0() }
case class L2Norm(v: N1) extends UnaryOp[S0, S1] { val shape: S0 = Shape0() }

// Dot
case class Dot11(l: N1, r: N1) extends BinaryOp[S0, S1, S1] { val shape: S0 = Shape0() }

// order 0 value is assumed to be expand to order 1 with same shape.
case class Dot01(l: N0, r: N1) extends BinaryOp[S0, S0, S1] { val shape: S0 = Shape0() }
case class Dot10(l: N1, r: N0) extends BinaryOp[S0, S1, S0] { val shape: S0 = Shape0() }

// Matmul
// assume that l is a row vector
case class MatMulR12(l: N1, r: N2) extends BinaryOp[S1, S1, S2] {
  val shape: S1 = Shape1(r.shape._2, transposed = true)
}

// assume that l is a column vector (so r shaped (1, k) is only allowed)
case class MatMulC12(l: N1, r: N2) extends BinaryOp[S2, S1, S2] {
  val shape: S2 = Shape2(l.shape._1, r.shape._2)
}

// assume that r is a column vector
case class MatMul2C1(l: N2, r: N1) extends BinaryOp[S1, S2, S1] {
  val shape: S1 = Shape1(l.shape._1, transposed = false)
}

// assume that r is a row vector (so l shaped (k, 1) is only allowed)
case class MatMul2R1(l: N2, r: N1) extends BinaryOp[S2, S2, S1] {
  val shape: S2 = Shape2(l.shape._1, r.shape._1)
}

case class MatMul22(l: N2, r: N2) extends BinaryOp[S2, S2, S2] {
  val shape: S2 = Shape2(l.shape._1, r.shape._2)
}

