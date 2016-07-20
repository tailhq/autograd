package scalaad.impl.nd4j

import scalaad.Eval
import scalaad.graph._
import scalaad.impl.std.{StdElementwiseOp, StdElementwiseOpB, StdElementwiseOpC}


// copy from StdScalarEval
trait Nd4jScalarEval { self: Nd4jTensorEval with Nd4jValue =>

  implicit val eval_nd4j_scalar_double: Eval[Expr[Real], T0] = new Eval[Expr[Real], T0] {

    def eval(n: Expr[Real]): T0 = n.shape.order match {
      case 0 => n match {

        // Nullary op
        case a: Var                     => a.data.value[T0]
        case a: Const[Real] @unchecked  => a.data.value[T0]
        case a: Diag                    => a.diagVec.value[T0]
        case _: Eye                     => 1.0

        // Unary op
        case Sum1(v, axis) => v.eval[T].sumNumber().doubleValue()
        case Max1(v, axis) => v.eval[T].maxNumber().doubleValue()
        case Min1(v, axis) => v.eval[T].minNumber().doubleValue()

        // Binary op
        case Dot(l, r) => Nd4jUtil.dot(l.eval[T], r.eval[T])

        // Ternary op
        case ElementwiseWhere(cond, a, b) => {
          if (cond.eval[B0]) a.eval[T0] else b.eval[T0]
        }

        // Elementwise
        case e: Elementwise0[Real] @unchecked =>
          val f = StdElementwiseOp.nullary(e)
          f()

        case e: Elementwise1[Real, Real] @unchecked =>
          val f = StdElementwiseOp.unary(e)
          f(e.v.eval[T0])

        case e: Elementwise2[Real, Real] @unchecked =>
          val f = StdElementwiseOp.binary(e)
          f(e.l.eval[T0], e.r.eval[T0])

      }
    }
  }

  implicit val eval_nd4j_scalar_bool: Eval[Expr[Bool], B0] = new Eval[Expr[Bool], B0] {

    def eval(n: Expr[Bool]): B0 = n.shape.order match {
      case 0 => n match {
        case e: Comparison2[Real] @unchecked =>
          val f = StdElementwiseOpC.binary(e)
          f(e.l.eval[T0], e.r.eval[T0])

        case e: BooleanOp1 =>
          val f = StdElementwiseOpB.unary(e)
          f(e.v.eval[B0])

        case e: BooleanOp2 =>
          val f = StdElementwiseOpB.binary(e)
          f(e.l.eval[B0], e.r.eval[B0])
      }
    }
  }

}
