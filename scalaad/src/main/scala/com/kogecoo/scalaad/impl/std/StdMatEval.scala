package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.graph.bool._
import com.kogecoo.scalaad.impl.std.{StdUtil => U}
import com.kogecoo.scalaad.op._
import com.kogecoo.scalaad.op.bool._
import shapeless.Nat
import shapeless.Nat._2


trait StdMatEval { self: StdMatValue with StdVecValue with StdVecEval =>

  private[this] type BOp = (T0, T0) => T0

  //private[this] def map12row(l: V1, r: V2, f: BOp): T2 = r.eval[T2].map(l.eval[T1].zip(_).map { case (x, y) => f(x, y) })
  //private[this] def map21row(l: V2, r: V1, f: BOp): T2 = l.eval[T2].map(_.zip(r.eval[T1]).map { case (x, y) => f(x, y) })
  //private[this] def map12col(l: V1, r: V2, f: BOp): T2 = r.eval[T2].zip(l.eval[T1]).map { case (y, x) => y.map(f(_, x)) }
  //private[this] def map21col(l: V2, r: V1, f: BOp): T2 = l.eval[T2].zip(r.eval[T1]).map { case (x, y) => x.map(f(_, y)) }

  //private[this] def map12(l: V1, r: V2, f: BOp): T2 = if (l.shape.transposed) map12row(l, r, f) else map12col(l, r, f)
  //private[this] def map21(l: V2, r: V1, f: BOp): T2 = if (r.shape.transposed) map21row(l, r, f) else map21col(l, r, f)

  implicit val eval22_stdmat_double: Eval[V2, T2] = new Eval[V2, T2] {

    def eval(n: V2): T2 = n match {

      case a: Var[_2]    => a.data.value[T2]
      case _: Zero[_2]   => U.const2(0.0, n.shape)
      case _: Half[_2]   => U.const2(0.5, n.shape)
      case _: One[_2]    => U.const2(1.0, n.shape)
      case a: Const[_2]  => a.data.value[T2]

      case a: Apply0[_2] => a.op match {
        case ZeroOp             => U.const2(0.0, n.shape)
        case HalfOp             => U.const2(0.5, n.shape)
        case OneOp              => U.const2(1.0, n.shape)
        case EyeOp              => U.const2(1.0, n.shape)
        case c: ConstOp[Nat._2] => c.v.value[T2]
        case c: DiagOp          => U.diag(c.v.value[T1])
      }

      // Unary ops
      case Elementwise1(v, op) => {
        val x = v.eval[T2]
        op match {
          case Pos      => U.broadcast2(x, +_)
          case Neg      => U.broadcast2(x, -_)
          case Identity => x
          case Sign     => U.broadcast2(x, a => math.abs(a) / a)

          case Sin => U.broadcast2(x, math.sin)
          case Cos => U.broadcast2(x, math.cos)
          case Tan => U.broadcast2(x, math.tan)

          case Asin => U.broadcast2(x, math.asin)
          case Acos => U.broadcast2(x, math.acos)
          case Atan => U.broadcast2(x, math.atan)

          case Sinh => U.broadcast2(x, math.sinh)
          case Cosh => U.broadcast2(x, math.cosh)
          case Tanh => U.broadcast2(x, math.tanh)

          case Ln   => U.broadcast2(x, math.log)
          case Exp  => U.broadcast2(x, math.exp)
          case Sqrt => U.broadcast2(x, math.sqrt)

          case Abs  => U.broadcast2(x, math.abs)
        }
      }

      case Elementwise2(l, r, op) => {
        val x = l.eval[T2]
        val y = r.eval[T2]
        op match {
          case Add => U.elementwise2(x, y, _ + _)
          case Sub => U.elementwise2(x, y, _ + _)
          case Mul => U.elementwise2(x, y, _ + _)
          case Div => U.elementwise2(x, y, _ + _)

          case Pow => U.elementwise2(x, y, math.pow)
          case Max2 => U.elementwise2(x, y, math.max)
          case Min2 => U.elementwise2(x, y, math.min)
        }
      }

      case ElementwiseWhere(cond, a, b) => {
        val z = cond.eval[Mat[Boolean]]
        val x = a.eval[T2]
        val y = b.eval[T2]
        z.zip(x.zip(y)).map { case (c, (d, e)) =>
          c.zip(d.zip(e)).map { case (f, (g, h)) =>
            if (f) g else h
          }
        }
      }

      case MatMul(a: V2, b: V2) => {  //assert(a(0).size == b.size)
        val aeval = a.eval[T2]
        val beval = b.eval[T2]

        assert(aeval.head.size == beval.size)
        (0 until b.shape.at(1)).map { bcolIndex =>
          aeval.map { arow =>
            arow.zip(beval.map(_.apply(bcolIndex))).map({ case (x, y) => x * y }).sum
          }
        }
      }
    }
  }

  implicit val eval_bool22_stdmat_double: Eval[B2, Mat[Boolean]] = new Eval[B2, Mat[Boolean]] {

    type BM = Mat[Boolean]

    def eval(n: B2): BM = n match {
      case Elementwise1B(v, op) => op match {
        case Not => U.broadcast2B(v.eval[BM], !_)
      }

      case Elementwise2B(l, r, op) => op match {
        case And => U.elementwise2B(l.eval[BM], r.eval[BM], _ && _)
        case Or  => U.elementwise2B(l.eval[BM], r.eval[BM], _ || _)
      }

      case Elementwise2C(l, r, op) => {
        val x = l.eval[T2]
        val y = r.eval[T2]
        op match {

          case Eq  => U.elementwise2C(x, y, _ == _)
          case Neq => U.elementwise2C(x, y, _ != _)
          case Lt  => U.elementwise2C(x, y, _ <  _)
          case Lte => U.elementwise2C(x, y, _ <= _)
          case Gt  => U.elementwise2C(x, y, _ >  _)
          case Gte => U.elementwise2C(x, y, _ >= _)

        }
      }
    }
  }

}
