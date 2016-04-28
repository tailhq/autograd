package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.graph.{Apply0, Apply00, V0}

trait Op0 {
  val child: Operator
}

case object Sin0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Sin0) }

case object Cos0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Cos0) }

case object Tan0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Tan0) }

case object Asin0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Asin0) }

case object Acos0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Acos0) }

case object Atan0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Atan0) }

case object Sinh0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Sinh0) }

case object Cosh0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Cosh0) }

case object Tanh0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Tanh0) }

case object Ln0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Ln0) }

case object Exp0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Exp0) }

case object Sqrt0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Sqrt0) }

case object Pow00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Pow00) }

case object Abs0 extends Op0  { def apply(v: V0): V0 = Apply0(v, Abs0) }

case object Max00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Max00) }

case object Min00 extends Op00 { def apply(l: V0, r: V0): V0 = Apply00(l, r, Min00) }


// Experimental

case object L0Norm extends Op1

case object L1Norm extends Op1

case object L2Norm extends Op1


case object Max1 extends Op1

case object Max2 extends Op2

case object Min1 extends Op2

case object Min2 extends Op2

case object Dot11 extends Op11

case object MatMul22 extends Op22

