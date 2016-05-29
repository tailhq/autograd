package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.{S0, S1, S2}


// Expr[Shape0] -> Expr[Shape0]

case object Sin  extends Op0

case object Cos  extends Op0

case object Tan  extends Op0

case object Asin extends Op0

case object Acos extends Op0

case object Atan extends Op0

case object Sinh extends Op0

case object Cosh extends Op0

case object Tanh extends Op0

case object Ln   extends Op0

case object Exp  extends Op0

case object Sqrt extends Op0

case object Abs  extends Op0


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

case object Pow extends Op00

case object Max extends Op00

case object Min extends Op00


// (Expr[Shape1], Expr[Shape1]) -> Expr[Shape0]

case object Dot extends BinaryOp[S0, S1, S1]


// (Expr[Shape2], Expr[Shape2]) -> Expr[Shape2]

case object MatMul extends Op22
