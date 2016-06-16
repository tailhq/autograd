package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool.{Elementwise1B, Elementwise2B, Elementwise2C}
import com.kogecoo.scalaad.impl.std.{StdUtil => U}
import com.kogecoo.scalaad.op._
import com.kogecoo.scalaad.op.bool._
import shapeless.Nat
import shapeless.Nat._1


trait StdVecEval { self: StdMatEval with StdVecValue =>

  implicit val eval_stdvec_double: Eval[V1, T1] = new Eval[V1, T1] {

    def eval(n: V1): T1 = n match {

      case a: Var[_1]    => a.data.value[T1]
      case _: Zero[_1]   => U.const1(0.0, n.shape)
      case _: Half[_1]   => U.const1(0.5, n.shape)
      case _: One[_1]    => U.const1(1.0, n.shape)
      case a: Const[_1]  => a.data.value[T1]

      case a: Apply0[_1] => a.op match {
        case ZeroOp             => U.const1(0.0, n.shape)
        case HalfOp             => U.const1(0.5, n.shape)
        case OneOp              => U.const1(1.0, n.shape)
        case EyeOp              => U.const1(1.0, n.shape)
        case c: ConstOp[Nat._1] => c.v.value[T1]
        case c: DiagOp          => c.v.value[T1]
      }

      // Unary ops
      case Elementwise1(v, op) => {
        val x = v.eval[T1]
        op match {
          case Pos      => U.broadcast1(x, +_)
          case Neg      => U.broadcast1(x, -_)
          case Identity => x
          case Sign     => U.broadcast1(x, a => math.abs(a) / a)

          case Sin => U.broadcast1(x, math.sin)
          case Cos => U.broadcast1(x, math.cos)
          case Tan => U.broadcast1(x, math.tan)

          case Asin => U.broadcast1(x, math.asin)
          case Acos => U.broadcast1(x, math.acos)
          case Atan => U.broadcast1(x, math.atan)

          case Sinh => U.broadcast1(x, math.sinh)
          case Cosh => U.broadcast1(x, math.cosh)
          case Tanh => U.broadcast1(x, math.tanh)

          case Ln   => U.broadcast1(x, math.log)
          case Exp  => U.broadcast1(x, math.exp)
          case Sqrt => U.broadcast1(x, math.sqrt)

          case Abs  => U.broadcast1(x, math.abs)
        }
      }

     // Binary ops

      case Elementwise2(l, r, op) => {
        val x = l.eval[T1]
        val y = r.eval[T1]
        op match {
          case Add => U.elementwise1(x, y, _ + _)
          case Sub => U.elementwise1(x, y, _ + _)
          case Mul => U.elementwise1(x, y, _ + _)
          case Div => U.elementwise1(x, y, _ + _)

          case Pow => U.elementwise1(x, y, math.pow)
          case Max2 => U.elementwise1(x, y, math.max)
          case Min2 => U.elementwise1(x, y, math.min)
        }
      }

      case ElementwiseWhere(cond, a, b) => {
        val z = cond.eval[Vec[Boolean]]
        val x = a.eval[T1]
        val y = b.eval[T1]
        z.zip(x.zip(y)).map { case (c, (d, e)) => if (c) d else e }

      }

      case Fold1(v, op, _) => {
        val x = v.eval[T2]
        op match {
          case Sum1 => x.map(_.sum)
          case Max1 => x.map(_.max)
          case Min1 => x.map(_.min)
        }
      }

    }
  }


  implicit val eval_bool_stdvec_double: Eval[B1, Vec[Boolean]] = new Eval[B1, Vec[Boolean]] {

    type BV = Vec[Boolean]

    def eval(n: B1): BV = n match {
      case Elementwise1B(v, op) => op match {
        case Not => U.broadcast1B(v.eval[BV], !_)
      }

      case Elementwise2B(l, r, op) => op match {
        case And => U.elementwise1B(l.eval[BV], r.eval[BV], _ && _)
        case Or  => U.elementwise1B(l.eval[BV], r.eval[BV], _ || _)
      }

      case Elementwise2C(l, r, op) => {
        val x = l.eval[T1]
        val y = r.eval[T1]
        op match {

          case Eq  => U.elementwise1C(x, y, _ == _)
          case Neq => U.elementwise1C(x, y, _ != _)
          case Lt  => U.elementwise1C(x, y, _ <  _)
          case Lte => U.elementwise1C(x, y, _ <= _)
          case Gt  => U.elementwise1C(x, y, _ >  _)
          case Gte => U.elementwise1C(x, y, _ >= _)


        }
      }
    }
  }

}
