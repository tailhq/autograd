package scalaad.impl.nd4j

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.api.ops.{TransformOp, impl => nd4jops}
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._

import scalaad.graph._
import scalaad.{Eval, NotImplementedYet}


trait Nd4jTensorEval { self: Nd4jValue =>

  implicit val eval_nd4j_tensor_double: Eval[Expr, T] = new Eval[Expr, T] {

    private[this] def exec(op: TransformOp): INDArray = {
      Nd4j.getExecutioner.execAndReturn(op)
    }


    def eval(n: Expr): T = n.shape.order match {
      case s if s > 0 => n match {

        // Nullary op
        case a: Var     => a.data.value[T]
        case a: Const   => a.data.value[T]
        case Diag(d, _) => Nd4j.diag(d.value[T])
        case _: Eye     => Nd4j.eye(n.shape.at(0))

        // Unary op
        case Sum1(v, axis) => v.eval[T].sum(axis)
        case Max1(v, axis) => v.eval[T].max(axis)
        case Min1(v, axis) => v.eval[T].min(axis)

        // Binary op
        case Dot(l, r)    => throw new NotImplementedYet()
        case MatMul(l, r) => l.eval[T].mmul(r.eval[T])


        case ElementwiseWhere(cond, a, b) => Nd4jUtil.where(cond.eval[T], a.eval[T], b.eval[T])

        // Elementwise
        case Zero(_) => Nd4j.zeros(n.shape.underlying: _*)
        case Half(_) => Nd4j.ones(n.shape.underlying: _*).div(2.0)
        case One (_) => Nd4j.ones(n.shape.underlying: _*)

        case Pos(v)      => v.eval[T]
        case Neg(v)      => exec(new nd4jops.transforms.Negative(v.eval[T].dup))
        case Identity(v) => v.eval[T]
        case Sign(v)     => exec(new nd4jops.transforms.Sign(v.eval[T].dup))

        case Sin(v) => exec(new nd4jops.transforms.Sin(v.eval[T].dup))
        case Cos(v) => exec(new nd4jops.transforms.Cos(v.eval[T].dup))
        case Tan(v) => throw new NotImplementedError()

        case Asin(v) => exec(new nd4jops.transforms.ASin(v.eval[T].dup))
        case Acos(v) => exec(new nd4jops.transforms.ACos(v.eval[T].dup))
        case Atan(v) => exec(new nd4jops.transforms.ATan(v.eval[T].dup))

        case Sinh(v) => throw new NotImplementedError()
        case Cosh(v) => throw new NotImplementedError()
        case Tanh(v) => exec(new nd4jops.transforms.Tanh(v.eval[T].dup))

        case Ln(v)   => exec(new nd4jops.transforms.Log(v.eval[T].dup))
        case Exp(v)  => exec(new nd4jops.transforms.Exp(v.eval[T].dup))
        case Sqrt(v) => exec(new nd4jops.transforms.Sqrt(v.eval[T].dup))

        case Abs(v)  => exec(new nd4jops.transforms.Abs(v.eval[T].dup))

        // Binary op
        case Add(l, r) => l.eval[T].add(r.eval[T])
        case Sub(l, r) => l.eval[T].sub(r.eval[T])
        case Mul(l, r) => l.eval[T].mul(r.eval[T])
        case Div(l, r) => l.eval[T].div(r.eval[T])

        case Pow (l, r) => Nd4jUtil.pow(l.eval[T], r.eval[T])
        case Max2(l, r) => {
          val x = l.eval[T]
          val y = r.eval[T]
          Nd4jUtil.where(Nd4jUtil.gte(x, y), x, y)
        }
        case Min2(l, r) => {
          val x = l.eval[T]
          val y = r.eval[T]
          Nd4jUtil.where(Nd4jUtil.lte(x, y), x, y)
        }

      /*}
    }
  }


  implicit val eval_ndj4_tensor_bool: Eval[Expr, T] = new Eval[Expr, T] {

    def eval(n: Expr): T = n.shape.order match {
      case s if s > 0 => n match {*/
        case Eq (l, r) => Nd4jUtil.eq (l.eval[T], r.eval[T])
        case Neq(l, r) => Nd4jUtil.neq(l.eval[T], r.eval[T])
        case Lt (l, r) => Nd4jUtil.lt (l.eval[T], r.eval[T])
        case Lte(l, r) => Nd4jUtil.lte(l.eval[T], r.eval[T])
        case Gt (l, r) => Nd4jUtil.gt (l.eval[T], r.eval[T])
        case Gte(l, r) => Nd4jUtil.gte(l.eval[T], r.eval[T])

        case Not(v)    => v.eval[INDArray].map { a => if (a == 0) 1.0 else 0.0 }
        case And(l, r) => Nd4jUtil.and(l.eval[INDArray], r.eval[INDArray])
        case Or(l, r)  => Nd4jUtil.or(l.eval[INDArray], r.eval[INDArray])
      }
    }
  }

}

