package scalaad.impl.std

import scalaad.Eval
import scalaad.graph._
import scalaad.impl.std.{StdUtil => U}


trait StdMatEval { self: StdMatValue with StdVecValue with StdVecEval with StdScalarEval =>

  implicit val eval22_stdmat_double: Eval[Expr, T2] = new Eval[Expr, T2] {

    def eval(n: Expr): T2 = n.shape.order match {
      case 2 => n match {

       // Nullary op
        case a: Var     => a.data.value[T2]
        case a: Const   => a.data.value[T2]
        case Diag(d, _) => d.value[T2]
        case _: Eye     => U.const2(1.0, n.shape)


        // Binary op
        case MatMul(a: Expr, b: Expr) => {
          val aeval = a.eval[T2]
          val beval = b.eval[T2]

          assert(aeval.head.size == beval.size)
          (0 until b.shape.at(1)).map { bcolIndex =>
            aeval.map { arow =>
              arow.zip(beval.map(_.apply(bcolIndex))).map({ case (x, y) => x * y }).sum
            }
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

        // Elementwise
        case e: Elementwise0 =>
          val f = StdElementwiseOp.nullary(e)
          U.const2(f(), e.shape)

        case e: Elementwise1 =>
          val f = StdElementwiseOp.unary(e)
          StdBroadcastHelper.unary2(e, f)

        case e: Elementwise2 =>
          val f = StdElementwiseOp.binary(e)
          StdBroadcastHelper.binary2(e, f)
      }
    }

  }

  implicit val eval_bool22_stdmat_double: Eval[Expr, B2] = new Eval[Expr, B2] {

    def eval(n: Expr): B2 = n.shape.order match {
      case 2 => n match {
        case e: Elementwise2 with Differentiable =>
          val f = StdElementwiseOpC.binary(e)
          StdBroadcastHelper.binary2(e, f)

        case e: Elementwise1 =>
          val f = StdElementwiseOpB.unary(e)
          StdBroadcastHelper.unary2(e, f)

        case e: Elementwise2 =>
          val f = StdElementwiseOpB.binary(e)
          StdBroadcastHelper.binary2(e, f)

      }
    }
  }

}
