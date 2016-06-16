package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool._
import com.kogecoo.scalaad.impl.std.{StdUtil => U}
import com.kogecoo.scalaad.op._
import com.kogecoo.scalaad.op.bool._
import shapeless.Nat
import shapeless.Nat._0


trait StdScalarEval { self: StdVecEval with StdScalarValue =>

  implicit val eval_double: Eval[V0, Double] = new Eval[V0, Double] {

    def eval(n: V0): T0 = n match {

      case a: Var[_0]   => a.data.value[T0]
      case _: Zero[_0]  => 0.0
      case _: Half[_0]  => 0.5
      case _: One[_0]   => 1.0
      case a: Const[_0] => a.data.value[T0]

      case Apply0(_, op) => op match {
        case ZeroOp             => 0.0
        case HalfOp             => 0.5
        case OneOp              => 1.0
        case EyeOp              => 1.0
        case c: ConstOp[Nat._0] => c.v.value[T0]
      }

      // Unary ops
      case Elementwise1(v, op) => {
        val x = v.eval[T0]
        op match {
          case Pos      => +x
          case Neg      => -x
          case Identity =>  x
          case Sign     => math.abs(x) / x

          case Sin => math.sin(x)
          case Cos => math.cos(x)
          case Tan => math.tan(x)

          case Asin => math.asin(x)
          case Acos => math.acos(x)
          case Atan => math.atan(x)

          case Sinh => math.sinh(x)
          case Cosh => math.cosh(x)
          case Tanh => math.tanh(x)

          case Ln   => math.log(x)
          case Exp  => math.exp(x)
          case Sqrt => math.sqrt(x)

          case Abs  => math.abs(x)
        }
      }

      // Binary ops
      case Elementwise2(l, r, op) => {
        val x = l.eval[T0]
        val y = r.eval[T0]
        op match {
          case Add => x + y
          case Sub => x - y
          case Mul => x * y
          case Div => x / y

          case Pow  => math.pow(x, y)
          case Max2 => math.max(x, y)
          case Min2 => math.min(x, y)
        }
      }

      case ElementwiseWhere(cond, a, b) => {
        if (cond.eval[Boolean]) a.eval[T0] else b.eval[T0]
      }

      case Fold1(v, op, _) => {
        val x = v.eval[T1]
        op match {
          case Sum1 => x.sum
          case Max1 => x.max
          case Min1 => x.min
        }
      }

      case Fold2(l, r, op, axis) => {
        val x = l.eval[T1]
        val y = r.eval[T1]
        op match {
          case Dot  => U.elementwise1(x, y, _ * _).sum
        }
      }


    }
  }

  implicit val eval_bool_double: Eval[B0, Boolean] = new Eval[B0, Boolean] {

    type B = Boolean

    def eval(n: B0): Boolean = n match {
      case Elementwise2C(l, r, op) => op match {
        case Eq  => l.eval[T0] == r.eval[T0]
        case Neq => l.eval[T0] != r.eval[T0]
        case Lt  => l.eval[T0] <  r.eval[T0]
        case Lte => l.eval[T0] <= r.eval[T0]
        case Gt  => l.eval[T0] >  r.eval[T0]
        case Gte => l.eval[T0] >= r.eval[T0]
      }
      case Elementwise1B(v, op) => op match {
        case Not => !v.eval[B]
      }

      case Elementwise2B(l, r, op) => op match {
        case And => l.eval[B] && r.eval[B]
        case Or  => l.eval[B] || r.eval[B]
      }
    }
  }

}
