package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.std.{StdUtil => U}


trait StdVecEval { self: StdMatEval with StdVecValue with StdScalarEval =>

  implicit val eval_std_vector_double: Eval[Expr, T1] = new Eval[Expr, T1] {

    def eval(n: Expr): T1 = n.shape.order match {
      case 1 => n match {

        // Nullary op
        case a: Var     => a.data.value[T1]
        case a: Const   => a.data.value[T1]
        case Diag(d, _) => d.value[T1]
        case _: Eye     => U.const1(1.0, n.shape)

        // Unary op
        case Sum1(v, axis) => U.fold2[T0, T0](v.eval[T2], _ + _, 0.0, axis)
        case Max1(v, axis) => U.fold2[T0, T0](v.eval[T2], math.max, Double.MinValue, axis)
        case Min1(v, axis) => U.fold2[T0, T0](v.eval[T2], math.min, Double.MaxValue, axis)

          // FIXME
        case ElementwiseWhere(cond, a, b) => {
          val z = cond.eval[B1]
          val x = a.eval[T1]
          val y = b.eval[T1]
          z.zip(x.zip(y)).map { case (c, (d, e)) => if (c) d else e }
        }

        // Elementwise
        case e: Elementwise0 =>
          val f = StdElementwiseOp.nullary(e)
          U.const1(f(), e.shape)

        case e: Elementwise1 =>
          val f = StdElementwiseOp.unary(e)
          StdBroadcastHelper.unary1(e, f)

        case e: Elementwise2 =>
          val f = StdElementwiseOp.binary(e)
          StdBroadcastHelper.binary1(e, f)

      }
    }
  }


  implicit val eval_std_vector_bool: Eval[Expr, B1] = new Eval[Expr, B1] {

    def eval(n: Expr): B1 = n.shape.order match {
      case 1 => n match {
        case e: Elementwise2 with Differentiable =>
          val f = StdElementwiseOpC.binary(e)
          StdBroadcastHelper.binary1(e, f)

        case e: Elementwise1 =>
          val f = StdElementwiseOpB.unary(e)
          StdBroadcastHelper.unary1(e, f)

        case e: Elementwise2 =>
          val f = StdElementwiseOpB.binary(e)
          StdBroadcastHelper.binary1(e, f)


      }
    }
  }

}
