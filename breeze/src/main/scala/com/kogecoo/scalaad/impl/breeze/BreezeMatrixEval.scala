package com.kogecoo.scalaad.impl.breeze

import breeze.linalg.DenseMatrix
import breeze.{numerics => bmath}
import com.kogecoo.scalaad.Eval

import scala.Predef.{any2stringadd => _, _}
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.breeze.Implicits._


trait BreezeMatrixEval {

  /*
  private[this] type T = Double
  private[this] type V = DenseMatrix[Double]

  implicit val eval22_breeze_matrix_double: Eval[V2, V] = new Eval[V2, V] {

    private[this] type N = V2

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var2(v, _)       => v.value[V]
      case Zero2(shape)     => DenseMatrix.zeros(shape._1, shape._2)
      case Half2(shape)     => DenseMatrix.fill(shape._1, shape._2)(0.5)
      case One2(shape)      => DenseMatrix.ones(shape._1, shape._2)
      case Const2(v, shape) => v.value[V]

      // Unary ops
      case Pos2(v) => v.eval[V].map { a => +a }
      case Neg2(v) => -v.eval[V]
      case Transpose2(v) => v.eval[V].t

      // Binary ops
      case Add02(l: V0, r: N)  => r.eval[V] + l.eval[T]  // FIXME: this assuming commutative law
      case Add20(l: N , r: V0) => l.eval[V] :+ r.eval[T]
      case Add22(l: N , r: N)  => l.eval[V] :+ r.eval[V]

      case Sub02(l: V0, r: N)  => l.eval[T]  - r.eval[V]
      case Sub20(l: N , r: V0) => l.eval[V] :- r.eval[T]
      case Sub22(l: N , r: N)  => l.eval[V] :- r.eval[V]

      case Mul02(l: V0, r: N)  => l.eval[T]  * r.eval[V]
      case Mul20(l: N , r: V0) => l.eval[V] :* r.eval[T]
      case Mul22(l: N , r: N)  => l.eval[V] :* r.eval[V]

      case Div02(l: V0, r: N)  => l.eval[T]  / r.eval[V]
      case Div20(l: N , r: V0) => l.eval[V] :/ r.eval[T]
      case Div22(l: N , r: N)  => l.eval[V] :/ r.eval[V]

      // Math
      case Sin2(v) =>  bmath.sin(v.eval[V])
      case Cos2(v) =>  bmath.cos(v.eval[V])
      case Tan2(v) =>  bmath.tan(v.eval[V])

      case Asin2(v) => bmath.asin(v.eval[V])
      case Acos2(v) => bmath.acos(v.eval[V])
      case Atan2(v) => bmath.atan(v.eval[V])

      case Sinh2(v) => bmath.sinh(v.eval[V])
      case Cosh2(v) => bmath.cosh(v.eval[V])
      case Tanh2(v) => bmath.tanh(v.eval[V])

      case Ln2(v)              => bmath.log(v.eval[V])
      case Exp2(v: N)          => bmath.exp(v.eval[V])
      case Sqrt2(v: N)         => bmath.sqrt(v.eval[V])
      case Pow02(l: V0, r: N)  => bmath.pow(l.eval[T], r.eval[V])
      case Pow20(l: N , r: V0) => bmath.pow(l.eval[V], r.eval[T])
      case Pow22(l: N , r: N)  => bmath.pow(l.eval[V], r.eval[V])

      case Abs2(v)                => v.eval[V].map(math.abs)
      case Max02(l: V0, r: N)  => breeze.linalg.max(r.eval[V], l.eval[T])
      case Max20(l: N , r: V0) => breeze.linalg.max(l.eval[V], r.eval[T])
      case Max22(l: N , r: N)  => breeze.linalg.max(l.eval[V], r.eval[V])
      case Min02(l: V0, r: N)  => breeze.linalg.min(r.eval[V], l.eval[T])
      case Min20(l: N , r: V0) => breeze.linalg.min(l.eval[V], r.eval[T])
      case Min22(l: N , r: N)  => breeze.linalg.min(l.eval[V], r.eval[V])
    }
  }


  implicit val eval_bool22_breeze_matrix_double: Eval[B2, DenseMatrix[Boolean]] = new Eval[B2, DenseMatrix[Boolean]] {

    private[this] def map20(a: DenseMatrix[Boolean], b: Boolean, op: (Boolean, Boolean) => Boolean): DenseMatrix[Boolean] = {
      a.map(op(_, b))
    }

    def eval(n: B2): DenseMatrix[Boolean] = n match {
      case Eq02 (l: V0, r: V2) => r.eval[V] :== l.eval[T]
      case Eq20 (l: V2, r: V0) => l.eval[V] :== r.eval[T]
      case Eq22 (l: V2, r: V2) => l.eval[V] :== r.eval[V]
      case Neq02(l: V0, r: V2) => r.eval[V] :!= l.eval[T]
      case Neq20(l: V2, r: V0) => l.eval[V] :!= r.eval[T]
      case Neq22(l: V2, r: V2) => l.eval[V] :!= r.eval[V]
      case Lt02 (l: V0, r: V2) => r.eval[V] :>  l.eval[T]
      case Lt20 (l: V2, r: V0) => l.eval[V] :<  r.eval[T]
      case Lt22 (l: V2, r: V2) => l.eval[V] :<  r.eval[V]
      case Lte02(l: V0, r: V2) => r.eval[V] :>= l.eval[T]
      case Lte20(l: V2, r: V0) => l.eval[V] :<= r.eval[T]
      case Lte22(l: V2, r: V2) => l.eval[V] :<= r.eval[V]
      case Gt02 (l: V0, r: V2) => r.eval[V] :<  l.eval[T]
      case Gt20 (l: V2, r: V0) => l.eval[V] :>  r.eval[T]
      case Gt22 (l: V2, r: V2) => l.eval[V] :>  r.eval[V]
      case Gte02(l: V0, r: V2) => r.eval[V] :<= l.eval[T]
      case Gte20(l: V2, r: V0) => l.eval[V] :>= r.eval[T]
      case Gte22(l: V2, r: V2) => l.eval[V] :>= r.eval[V]

      case And02(l: B0, r: B2) => map20(r.eval[DenseMatrix[Boolean]], l.eval[Boolean], _ && _)
      case And20(l: B2, r: B0) => map20(l.eval[DenseMatrix[Boolean]], r.eval[Boolean], _ && _)
      case And22(l: B2, r: B2) => l.eval[DenseMatrix[Boolean]] :& r.eval[DenseMatrix[Boolean]]
      case Or02 (l: B0, r: B2) => map20(r.eval[DenseMatrix[Boolean]], l.eval[Boolean], _ || _)
      case Or20 (l: B2, r: B0) => map20(l.eval[DenseMatrix[Boolean]], r.eval[Boolean], _ || _)
      case Or22 (l: B2, r: B2) => l.eval[DenseMatrix[Boolean]] :| r.eval[DenseMatrix[Boolean]]

      case Not2(v: B2) => !v.eval[DenseMatrix[Boolean]]
    }
  }
  */

}
