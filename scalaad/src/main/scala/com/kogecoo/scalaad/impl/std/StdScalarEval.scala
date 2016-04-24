package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.Implicits._
import com.kogecoo.scalaad.impl.std.StdUtil.{T0, T1}
import com.kogecoo.scalaad.impl.std.{StdUtil => U}


trait StdScalarEval {// self: StdVecEval =>

  // operators/functions convert Node order 0 -> 0
  implicit val eval00_double: Eval[V0, Double] = new Eval[V0, Double] {

    def eval(n: V0): T0 = n match {

      // Leaf nodes
      case Var0(v)          => v.value[T0]
      case ArbVar0(_, data) => data.get.value[T0]
      case Zero0()          => 0.0
      case Half0()          => 0.5
      case One0()           => 1.0
      case Const0(v)        => v.value[T0]

      // Unary ops
      case Pos0(v: V0) => +v.eval[T0]
      case Neg0(v: V0) => -v.eval[T0]

      // Binary ops
      case Add00(l: V0, r: V0) => l.eval[T0] + r.eval[T0]
      case Sub00(l: V0, r: V0) => l.eval[T0] - r.eval[T0]
      case Mul00(l: V0, r: V0) => l.eval[T0] * r.eval[T0]
      case Div00(l: V0, r: V0) => l.eval[T0] / r.eval[T0]

      // Math
      case Sin0(v: V0) => math.sin(v.eval[T0])
      case Cos0(v: V0) => math.cos(v.eval[T0])
      case Tan0(v: V0) => math.tan(v.eval[T0])

      case Asin0(v: V0) => math.asin(v.eval[T0])
      case Acos0(v: V0) => math.acos(v.eval[T0])
      case Atan0(v: V0) => math.atan(v.eval[T0])

      case Sinh0(v: V0) => math.sinh(v.eval[T0])
      case Cosh0(v: V0) => math.cosh(v.eval[T0])
      case Tanh0(v: V0) => math.tanh(v.eval[T0])

      case Ln0(v: V0)         => math.log(v.eval[T0])
      case Exp0(v: V0)        => math.exp(v.eval[T0])
      case Sqrt0(v: V0)       => math.sqrt(v.eval[T0])
      case Pow00(l: V0, r: V0) => math.pow(l.eval[T0], r.eval[T0])

      // Experimental

      case Dot01(l: V0, r: V1) => U.broadcast1(r.eval[T1], l.eval[T0] * _).sum
      case Dot10(l: V1, r: V0) => U.broadcast1(l.eval[T1], _ * r.eval[T0]).sum
      case Dot11(l: V1, r: V1) => U.elementwise1(l.eval[T1], r.eval[T1], _ * _).sum

      case Abs0(v: V0)         => math.abs(v.eval[T0])
      case Max00(l: V0, r: V0) => math.max(l.eval[T0], r.eval[T0])
      case Min00(l: V0, r: V0) => math.min(l.eval[T0], r.eval[T0])

      case Where0_0(cond: B0, a: V0, b: V0) => if (cond.eval[Boolean]) a.eval[T0] else b.eval[T0]

      /*
      case L0Norm(v: N1) => v.eval[T1].count(_ != StdUtil.zero0)
      case L1Norm(v: N1) => StdUtil.broadcast1(v.eval[T1], math.abs).sum
      case L2Norm(v: N1) => StdUtil.broadcast1(v.eval[T1], (x: T0) => x * x).sum
      */
    }
  }

  implicit val eval_bool00_double: Eval[B0, Boolean] = new Eval[B0, Boolean] {

    type B = Boolean

    def eval(n: B0): Boolean = n match {
      case Eq00 (l: V0, r: V0) => l.eval[T0] == r.eval[T0]
      case Neq00(l: V0, r: V0) => l.eval[T0] != r.eval[T0]
      case Lt00 (l: V0, r: V0) => l.eval[T0] <  r.eval[T0]
      case Lte00(l: V0, r: V0) => l.eval[T0] <= r.eval[T0]
      case Gt00 (l: V0, r: V0) => l.eval[T0] >  r.eval[T0]
      case Gte00(l: V0, r: V0) => l.eval[T0] >= r.eval[T0]

      case And00(l: B0, r: B0) => l.eval[B] && r.eval[B]
      case Or00(l: B0, r: B0)  => l.eval[B] || r.eval[B]

      case Not0(v: B0) => !v.eval[B]
    }
  }

}
