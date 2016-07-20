package scalaad.impl.breeze

import breeze.linalg
import breeze.linalg.{*, DenseVector}
import breeze.numerics

import scala.Predef.{any2stringadd => _}
import scalaad.graph._
import scalaad.{Eval, NotImplementedYet}


trait BreezeVectorEval { self: BreezeMatrixEval with BreezeScalarEval with BreezeValue =>

  // FIXME: Broadcasting
  implicit val eval11_breeze_vector_double: Eval[Expr[Real], T1] = new Eval[Expr[Real], T1] {

    def eval(n: Expr[Real]): T1 = n.shape.order match {
      case 1 => n match {
        // Nullary op
        case a: Var                    => a.data.value[T1]
        case a: Const[Real] @unchecked => a.data.value[T1]
        case a: Diag                   => a.diagVec.value[T1]
        case _: Eye                    => DenseVector.ones[T0](n.shape.at(0))

        // Unary op
        case Sum1(v, axis) => {
          val x = v.eval[T2]
          axis match {
            case 0 => linalg.sum(x(*, ::))
            case 1 => linalg.sum(x(::, *)).inner.toDenseVector // FIXME
          }
        }

        case Max1(v, axis) => {
          val x = v.eval[T2]
          axis match {
            case 0 => linalg.max(x(*, ::))
            case 1 => linalg.min(x(::, *)).inner.toDenseVector
          }
        }

        case Min1(v, axis) => {
          val x = v.eval[T2]
          axis match {
            case 0 => linalg.max(x(*, ::))
            case 1 => linalg.min(x(::, *)).inner.toDenseVector
          }
        }

        // Binary op
        case Dot(l, r)    => throw new NotImplementedYet()

        // Ternary op
        case ElementwiseWhere(cond, a, b) => {
          val z = cond.eval[B1]
          val x = a.eval[T1]
          val y = b.eval[T1]
          z.mapPairs { case (k, v) => if (v) x(k) else y(k) }
        }

        // Elementwise
        case Zero(_) => DenseVector.zeros(n.shape.at(0))
        case Half(_) => DenseVector.fill(n.shape.at(0))(0.5)
        case One (_) => DenseVector.ones(n.shape.at(0))

        case Pos(v)      => v.eval[T1]
        case Neg(v)      => -v.eval[T1]
        case Identity(v) => v.eval[T1]
        case Sign(v)     => numerics.signum(v.eval[T1])

        case Sin(v) => numerics.sin(v.eval[T1])
        case Cos(v) => numerics.cos(v.eval[T1])
        case Tan(v) => numerics.tan(v.eval[T1])

        case Asin(v) => numerics.asin(v.eval[T1])
        case Acos(v) => numerics.acos(v.eval[T1])
        case Atan(v) => numerics.atan(v.eval[T1])

        case Sinh(v) => numerics.sinh(v.eval[T1])
        case Cosh(v) => numerics.cosh(v.eval[T1])
        case Tanh(v) => numerics.tanh(v.eval[T1])

        case Ln(v)   => numerics.log(v.eval[T1])
        case Exp(v)  => numerics.exp(v.eval[T1])
        case Sqrt(v) => numerics.sqrt(v.eval[T1])

        case Abs(v)  => numerics.abs(v.eval[T1])

        // Binary op
        case Add(l, r) => l.eval[T1] :+ r.eval[T1]
        case Sub(l, r) => l.eval[T1] :- r.eval[T1]
        case Mul(l, r) => l.eval[T1] :* r.eval[T1]
        case Div(l, r) => l.eval[T1] :/ r.eval[T1]

        case Pow (l, r) => numerics.pow(l.eval[T1], r.eval[T1])

        case Max2(l, r) => { // FIXME: UFunc
          val x = r.eval[T1]
          l.eval[T1].mapPairs { case (k, v) => math.max(v, x(k)) }
        }
        case Min2(l, r) => {
          val x = r.eval[T1]
          l.eval[T1].mapPairs { case (k, v) => math.min(v, x(k)) }
        }

      }
    }
  }

  implicit val eval_bool_breeze_vecotr_double: Eval[Expr[Bool], B1] = new Eval[Expr[Bool], B1] {

    def eval(n: Expr[Bool]): B1 = n.shape.order match {
      case 1 => n match {
        case Eq (l, r) => l.eval[T1] :== r.eval[T1]
        case Neq(l, r) => l.eval[T1] :!= r.eval[T1]
        case Lt (l, r) => l.eval[T1] :<  r.eval[T1]
        case Lte(l, r) => l.eval[T1] :<= r.eval[T1]
        case Gt (l, r) => l.eval[T1] :>  r.eval[T1]
        case Gte(l, r) => l.eval[T1] :>= r.eval[T1]

        case Not(v)    => !v.eval[B1]
        case And(l, r) => l.eval[B1] :& r.eval[B1]
        case Or(l, r)  => l.eval[B1] :| r.eval[B1]
      }
    }
  }

}

