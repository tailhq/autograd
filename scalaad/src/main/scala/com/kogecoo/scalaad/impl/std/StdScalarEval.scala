package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool.{Apply1B, Apply2B, Apply2C}
import com.kogecoo.scalaad.impl.std.{StdUtil => U}
import com.kogecoo.scalaad.op.bool.{And, Eq, Gt, Gte, Lt, Lte, Neq, Not, Or}
import com.kogecoo.scalaad.op.{Acos, Add, Asin, Atan, Cos, Cosh, Div, Exp, Ln, Mul, Neg, Pos, Sin, Sinh, Sqrt, Sub, Tan, Tanh}


trait StdScalarEval {// self: StdVecEval =>

  // operators/functions convert Node order 0 -> 0
  implicit val eval00_double: Eval[V0, Double] = new Eval[V0, Double] {

    def eval(n: V0): T0 = n match {

      // Leaf nodes
      /*case Var0(v)      => v.value[T0]
      case a: ArbVar0   => a.data.get.value[T0]
      case _: Zero[_0]  => 0.0
      case _: Half[_0]  => 0.5
      case _: One[_0]   => 1.0
      case c: Const[_0] => c.v.value[T0]
      */

      // Unary ops
      case Apply1(v, op) => op match {
        case Pos => +v.eval[T0]
        case Neg => -v.eval[T0]

        case Sin => math.sin(v.eval[T0])
        case Cos => math.cos(v.eval[T0])
        case Tan => math.tan(v.eval[T0])

        case Asin => math.asin(v.eval[T0])
        case Acos => math.acos(v.eval[T0])
        case Atan => math.atan(v.eval[T0])

        case Sinh => math.sinh(v.eval[T0])
        case Cosh => math.cosh(v.eval[T0])
        case Tanh => math.tanh(v.eval[T0])

        case Ln   => math.log(v.eval[T0])
        case Exp  => math.exp(v.eval[T0])
        case Sqrt => math.sqrt(v.eval[T0])

        //case Abs  => math.abs(v.eval[T0])
      }

      // Binary ops
      case Apply2(l, r, op) => op match {
        case Add => l.eval[T0] + r.eval[T0]
        case Sub => l.eval[T0] - r.eval[T0]
        case Mul => l.eval[T0] * r.eval[T0]
        case Div => l.eval[T0] / r.eval[T0]

        //case Pow => math.pow(l.eval[T0], r.eval[T0])

        // Experimental

        /*case Dot01(l: V0, r: V1) => U.broadcast1(r.eval[T1], l.eval[T0] * _).sum
        case Dot10(l: V1, r: V0) => U.broadcast1(l.eval[T1], _ * r.eval[T0]).sum
        case Dot11(l: V1, r: V1) => U.elementwise1(l.eval[T1], r.eval[T1], _ * _).sum

        case Max => math.max(l.eval[T0], r.eval[T0])
        case Min => math.min(l.eval[T0], r.eval[T0])
        */

      }
      //case Where0_0(cond: B0, a: V0, b: V0) => if (cond.eval[Boolean]) a.eval[T0] else b.eval[T0]
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
      case Apply2C(l, r, op) => op match {
        case Eq  => l.eval[T0] == r.eval[T0]
        case Neq => l.eval[T0] != r.eval[T0]
        case Lt  => l.eval[T0] <  r.eval[T0]
        case Lte => l.eval[T0] <= r.eval[T0]
        case Gt  => l.eval[T0] >  r.eval[T0]
        case Gte => l.eval[T0] >= r.eval[T0]
      }
      case Apply1B(v, op) => op match {
        case Not => !v.eval[B]
      }

      case Apply2B(l, r, op) => op match {
        case And => l.eval[B] && r.eval[B]
        case Or  => l.eval[B] || r.eval[B]
      }
    }
  }

}
