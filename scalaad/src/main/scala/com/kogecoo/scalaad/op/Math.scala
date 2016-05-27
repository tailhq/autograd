package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{S0, S1, S2}


// Expr[Shape0] -> Expr[Shape0]

case object Sin   extends UnaryOp[S0, S0]

case object Cos   extends UnaryOp[S0, S0]

case object Tan0  extends UnaryOp[S0, S0]

case object Asin0 extends UnaryOp[S0, S0]

case object Acos0 extends UnaryOp[S0, S0]

case object Atan0 extends UnaryOp[S0, S0]

case object Sinh0 extends UnaryOp[S0, S0]

case object Cosh0 extends UnaryOp[S0, S0]

case object Tanh0 extends UnaryOp[S0, S0]

case object Ln0   extends UnaryOp[S0, S0]

case object Exp0  extends UnaryOp[S0, S0]

case object Sqrt0 extends UnaryOp[S0, S0]

case object Abs0  extends UnaryOp[S0, S0]


// Expr[Shape1] -> Expr[Shape0]

case object L0Norm extends UnaryOp[S0, S1]

case object L1Norm extends UnaryOp[S0, S1]

case object L2Norm extends UnaryOp[S0, S1]

case object Sum1 extends UnaryOp[S0, S1]

case object Max1 extends UnaryOp[S0, S1]

case object Min1 extends UnaryOp[S0, S1]


// Expr[Shape2] -> Expr[Shape0]

case object Sum2 extends UnaryOp[S0, S2]

case object Max2 extends UnaryOp[S0, S2]

case object Min2 extends UnaryOp[S0, S2]



// (Expr[Shape0], Expr[Shape0]) -> Expr[Shape0]

case object Pow00 extends BinaryOp[S0, S0, S0]

case object Max00 extends BinaryOp[S0, S0, S0]

case object Min00 extends BinaryOp[S0, S0, S0]


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case object Dot11 extends BinaryOp[S0, S1, S1]


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case object MatMul22 extends BinaryOp[S2, S2, S2]
