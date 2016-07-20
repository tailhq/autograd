package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.{StdUtil => U}


trait StdScalarEval { self: StdVecEval with StdScalarValue =>

  implicit val eval_std_scalar_double: Eval[Expr, T0] = new Eval[Expr, T0] {

    def eval(n: Expr): T0 = n.shape.order match {
      case 0 => n match {

        // Nullary op
        case Var(d)     => d.value[T0]
        case Const(d)   => d.value[T0]
        case Diag(d, _) => d.value[T0]
        case _: Eye     => 1.0

        // Unary op
        case Sum1(v, axis) => U.fold1[T0, T0](v.eval[T1], _ + _, 0.0)  // FIXME: assert(axis == 0)
        case Max1(v, axis) => U.fold1[T0, T0](v.eval[T1], math.max, Double.MinValue) // FIXME: assert(axis == 0)
        case Min1(v, axis) => U.fold1[T0, T0](v.eval[T1], math.min, Double.MaxValue) // FIXME: assert(axis == 0)

        // Binary op
        // broadcast?
        case Dot(l, r) =>
          l.eval[T1].zip(r.eval[T1]).map({ case (a, b) => a * b}).foldLeft(0.0)(_ + _)

        case ElementwiseWhere(cond, a, b) =>
          if (cond.eval[B0]) a.eval[T0] else b.eval[T0]

        // Elementwise
        case e: Elementwise0 =>
          val f = StdElementwiseOp.nullary(e)
          f()

        case e: Elementwise1 =>
          val f = StdElementwiseOp.unary(e)
          f(e.v.eval[T0])

        case e: Elementwise2 =>
          val f = StdElementwiseOp.binary(e)
          f(e.l.eval[T0], e.r.eval[T0])
      }
    }
  }


  implicit val eval_std_scalar_bool: Eval[Expr, B0] = new Eval[Expr, B0] {

    def eval(n: Expr): B0 = n.shape.order match {
      case 0 => n match {
        case e: Elementwise2 with Differentiable =>
          val f = StdElementwiseOpC.binary(e)
          f(e.l.eval[T0], e.r.eval[T0])

        case e: Elementwise1 =>
          val f = StdElementwiseOpB.unary(e)
          f(e.v.eval[B0])

        case e: Elementwise2 =>
          val f = StdElementwiseOpB.binary(e)
          f(e.l.eval[B0], e.r.eval[B0])


      }
    }
  }

}
