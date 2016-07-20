package scalaad.impl.std

import scalaad.Eval
import scalaad.graph._
import scalaad.impl.std.{StdUtil => U}


trait StdVecEval { self: StdMatEval with StdScalarEval with StdValue =>

  implicit val eval_std_vector_double: Eval[Expr[Real], T1] = new Eval[Expr[Real], T1] {

    def eval(n: Expr[Real]): T1 = n.shape.order match {
      case 1 => n match {

        // Nullary op
        case a: Var                    => a.data.value[T1]
        case a: Const[Real] @unchecked => a.data.value[T1]
        case Diag(d, _)                => d.value[T1]
        case _: Eye                    => U.const1(1.0, n.shape)

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
        case e: Elementwise0[Real] @unchecked =>
          val f = StdElementwiseOp.nullary(e)
          U.const1(f(), e.shape)

        case e: Elementwise1[Real, Real] @unchecked =>
          val f = StdElementwiseOp.unary(e)
          StdBroadcastHelper.unary1(e, f)

        case e: Elementwise2[Real, Real] @unchecked =>
          val f = StdElementwiseOp.binary(e)
          StdBroadcastHelper.binary1[Real, Real, T0, T0](e, f)

      }
    }
  }


  implicit val eval_std_vector_bool: Eval[Expr[Bool], B1] = new Eval[Expr[Bool], B1] {

    def eval(n: Expr[Bool]): B1 = n.shape.order match {
      case 1 => n match {
        case e: Comparison2[Real] @unchecked =>
          val f = StdElementwiseOpC.binary(e)
          StdBroadcastHelper.binary1(e, f)

        case e: BooleanOp1 =>
          val f = StdElementwiseOpB.unary(e)
          StdBroadcastHelper.unary1(e, f)

        case e: BooleanOp2 =>
          val f = StdElementwiseOpB.binary(e)
          StdBroadcastHelper.binary1(e, f)


      }
    }
  }

}
