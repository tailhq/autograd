package com.kogecoo.scalaad.op

import shapeless.Nat


// Expr[Shape0] -> Expr[Shape0]

case object Sin  extends UnaryOp

case object Cos  extends UnaryOp

case object Tan  extends UnaryOp

case object Asin extends UnaryOp

case object Acos extends UnaryOp

case object Atan extends UnaryOp

case object Sinh extends UnaryOp

case object Cosh extends UnaryOp

case object Tanh extends UnaryOp

case object Ln   extends UnaryOp

case object Exp  extends UnaryOp

case object Sqrt extends UnaryOp

case object Abs  extends UnaryOp


// Expr[Shape1] -> Expr[Shape0]

case class L0Norm[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class L1Norm[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class L2Norm[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class Sum1[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class Max1[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class Min1[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]


// Expr[Shape2] -> Expr[Shape0]

case class Sum2[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class Max2[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]

case class Min2[O <: Nat, I <: Nat]() extends FoldUnaryOp[O, I]



// (Expr[Shape0], Expr[Shape0]) -> Expr[Shape0]

case object Pow extends BinaryOp

case object Max extends BinaryOp

case object Min extends BinaryOp


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case class Dot[O <: Nat, I <: Nat]() extends FoldBinaryOp[O, I]


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case class MatMul[O <: Nat, I <: Nat]() extends FoldBinaryOp[O, I]
