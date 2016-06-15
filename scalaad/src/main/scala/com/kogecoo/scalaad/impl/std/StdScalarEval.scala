package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool._
import com.kogecoo.scalaad.op.{Dot, _}
import com.kogecoo.scalaad.op.bool._
import shapeless.Nat._0


trait StdScalarEval {

  // operators/functions convert Node order 0 -> 0
  implicit val eval00_double: Eval[V0, Double] = new Eval[V0, Double] {

    def eval(n: V0): T0 = n match {

      // Leaf nodes

      case Var(v)       => v.value[T0]
      case a: ArbVar0   => a.data.get.value[T0]
      case _: Zero[_0]  => 0.0
      case _: Half[_0]  => 0.5
      case _: One[_0]   => 1.0
      case c: Const[_0] => c.v.value[T0]

      case Apply0(_, op) => op match {
        case ZeroOp     => 0.0
        case HalfOp     => 0.5
        case OneOp      => 1.0
        case EyeOp      => 1.0
        case ConstOp(v) => v.value[T0]
        case DiagOp(v)  => v.value[T0]

      }

      // Unary ops
      case Apply1(v, op) => op match {
        case Pos      => +v.eval[T0]
        case Neg      => -v.eval[T0]
        case Identity =>  v.eval[T0]
        case Sign     => math.abs(v.eval[T0]) / v.eval[T0]

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

        case Abs  => math.abs(v.eval[T0])
      }

      case Fold1(v, op, _) => op match {
        case Sum1 => v.eval[T0]
        case Max1 => v.eval[T0]
        case Min1 => v.eval[T0]
      }

      // Binary ops
      case Apply2(l, r, op) => op match {
        case Add => l.eval[T0] + r.eval[T0]
        case Sub => l.eval[T0] - r.eval[T0]
        case Mul => l.eval[T0] * r.eval[T0]
        case Div => l.eval[T0] / r.eval[T0]

        case Pow => math.pow(l.eval[T0], r.eval[T0])
        case Max => math.max(l.eval[T0], r.eval[T0])
        case Min => math.min(l.eval[T0], r.eval[T0])
      }

      case Fold2(l, r, op, axis) => op match {
        case Dot => StdUtil.broadcast1(r.eval[T1], l.eval[T0] * _).sum

        case Max2 => math.max(l.eval[T0], r.eval[T0])
        case Min2 => math.min(l.eval[T0], r.eval[T0])

      }

      case Where(cond, a, b) => if (cond.eval[Boolean]) a.eval[T0] else b.eval[T0]
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
