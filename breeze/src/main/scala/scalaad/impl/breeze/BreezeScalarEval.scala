package scalaad.impl.breeze

import breeze.linalg

import scalaad.Eval
import scalaad.graph._
import scalaad.impl.std.{StdElementwiseOp, StdElementwiseOpB, StdElementwiseOpC}


// copy from StdScalarEval
trait BreezeScalarEval { self: BreezeValue with BreezeVectorEval =>

  implicit val eval_breeze_scalar_double: Eval[Expr[Real], T0] = new Eval[Expr[Real], T0] {

    def eval(n: Expr[Real]): T0 = n.shape.order match {
      case 0 => n match {

        // Nullary op
        case a: Var                    => a.data.value[T0]
        case a: Const[Real] @unchecked => a.data.value[T0]
        case a: Diag                   => a.diagVec.value[T0]
        case _: Eye                    => 1.0

        case Sum1(v, axis) => linalg.sum(v.eval[T1])
        case Max1(v, axis) => linalg.max(v.eval[T1])
        case Min1(v, axis) => linalg.min(v.eval[T1])

        // Binary op
        case Dot(l, r) => l.eval[T1].dot(r.eval[T1])

        // Ternary op
        case ElementwiseWhere(cond, a, b) => {
          if (cond.eval[B0]) a.eval[T0] else b.eval[T0]
        }

        // Elementwise
        case e: Elementwise0[Real] =>
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

  implicit val eval_breeze_scalar_bool: Eval[Expr[Bool], B0] = new Eval[Expr[Bool], B0] {

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
