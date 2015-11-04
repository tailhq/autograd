package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph._
import com.kogecoo.scalaad.impl.nd4j.Implicits._

import scala.Predef.{any2stringadd => _, _}
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4s.Implicits._


trait Nd4jMatrixEval {

  private[this] type T = Double
  private[this] type V = INDArray

  implicit val eval22_nd4j_matrix_double: Eval[N2, INDArray] = new Eval[N2, INDArray] {

    private[this] type N = N2

    def eval(n: N): V = n match {

      // Leaf nodes
      case Var2(v, _)       => v.value[V]
      case Zero2(shape)     => Nd4j.zeros(shape._1, shape._2)
      case Half2(shape)     => Nd4j.ones(shape._1, shape._2) * 0.5
      case One2(shape)      => Nd4j.ones(shape._1, shape._2)
      case Const2(v, shape) => v.value[V]

      // Unary ops
      case Pos2(v) => v.eval[V].map { a => +a }
      case Neg2(v) => -v.eval[V]

      // Binary ops
      case Add02(l: N0, r: N)  => r.eval[V].add(l.eval[T])  // FIXME: this assuming commutative law
      case Add20(l: N , r: N0) => l.eval[V].add(r.eval[T])
      case Add22(l: N , r: N)  => l.eval[V].add(r.eval[V])

      case Sub02(l: N0, r: N)  => r.eval[V].rsub(l.eval[T])
      case Sub20(l: N , r: N0) => l.eval[V].sub(r.eval[T])
      case Sub22(l: N , r: N)  => l.eval[V].sub(r.eval[V])

      case Mul02(l: N0, r: N)  => r.eval[V].mul(l.eval[T])  // FIXME: this assuming commutative law
      case Mul20(l: N , r: N0) => l.eval[V].mul(r.eval[T])
      case Mul22(l: N , r: N)  => l.eval[V].mul(r.eval[V])

      case Div02(l: N0, r: N)  => r.eval[V].rdiv(l.eval[T])
      case Div20(l: N , r: N0) => l.eval[V].div(r.eval[T])
      case Div22(l: N , r: N)  => l.eval[V].div(r.eval[V])

      // Math
      // TODO: use executioner
      case Sin2(v) =>  v.eval[V].map(math.sin)
      case Cos2(v) =>  v.eval[V].map(math.cos)
      case Tan2(v) =>  v.eval[V].map(math.tan)

      case Asin2(v) => v.eval[V].map(math.asin)
      case Acos2(v) => v.eval[V].map(math.acos)
      case Atan2(v) => v.eval[V].map(math.atan)

      case Sinh2(v) => v.eval[V].map(math.sinh)
      case Cosh2(v) => v.eval[V].map(math.cosh)
      case Tanh2(v) => v.eval[V].map(math.tanh)

      case Ln2(v)              => v.eval[V].map(math.log)
      case Exp2(v: N)          => v.eval[V].map(math.exp)
      case Sqrt2(v: N)         => v.eval[V].map(math.sqrt)
      case Pow02(l: N0, r: N)  => Nd4jUtil.pow(l.eval[T], r.eval[V])
      case Pow20(l: N , r: N0) => Nd4jUtil.pow(l.eval[V], r.eval[T])
      case Pow22(l: N , r: N)  => Nd4jUtil.pow(l.eval[V], r.eval[V])

      case Abs2(v)                => v.eval[V].map(math.abs)
      case Max02(l: N0   , r: N)  => Nd4jUtil.max(r.eval[V], l.eval[T])
      case Max20(l: N    , r: N0) => Nd4jUtil.max(l.eval[V], r.eval[T])
      case Max22(l: N    , r: N)  => Nd4jUtil.max(l.eval[V], r.eval[V])
      case Min02(l: N0   , r: N)  => Nd4jUtil.min(r.eval[V], l.eval[T])
      case Min20(l: N    , r: N0) => Nd4jUtil.min(l.eval[V], r.eval[T])
      case Min22(l: N    , r: N)  => Nd4jUtil.min(l.eval[V], r.eval[V])
    }
  }


  implicit val eval_bool22_nd4j_matrix_double: Eval[B2, INDArray] = new Eval[B2, INDArray] {

    def eval(n: B2): INDArray = n match {
      case Eq02(l: N0, r: N2)  => Nd4jUtil.eq(r.eval[V], l.eval[T])
      case Eq20(l: N2, r: N0)  => Nd4jUtil.eq(l.eval[V], r.eval[T])
      case Eq22(l: N2, r: N2)  => Nd4jUtil.eq(l.eval[V], r.eval[V])
      case Neq02(l: N0, r: N2) => Nd4jUtil.neq(r.eval[V], l.eval[T])
      case Neq20(l: N2, r: N0) => Nd4jUtil.neq(l.eval[V], r.eval[T])
      case Neq22(l: N2, r: N2) => Nd4jUtil.neq(l.eval[V], r.eval[V])
      case Lt02(l: N0, r: N2)  => Nd4jUtil.gt(r.eval[V], l.eval[T])
      case Lt20(l: N2, r: N0)  => Nd4jUtil.lt(l.eval[V], r.eval[T])
      case Lt22(l: N2, r: N2)  => Nd4jUtil.lt(l.eval[V], r.eval[V])
      case Lte02(l: N0, r: N2) => Nd4jUtil.gte(r.eval[V], l.eval[T])
      case Lte20(l: N2, r: N0) => Nd4jUtil.lte(l.eval[V], r.eval[T])
      case Lte22(l: N2, r: N2) => Nd4jUtil.lte(l.eval[V], r.eval[V])
      case Gt02(l: N0, r: N2)  => Nd4jUtil.lt(r.eval[V], l.eval[T])
      case Gt20(l: N2, r: N0)  => Nd4jUtil.gt(l.eval[V], r.eval[T])
      case Gt22(l: N2, r: N2)  => Nd4jUtil.gt(l.eval[V], r.eval[V])
      case Gte02(l: N0, r: N2) => Nd4jUtil.gte(r.eval[V], l.eval[T])
      case Gte20(l: N2, r: N0) => Nd4jUtil.gte(l.eval[V], r.eval[T])
      case Gte22(l: N2, r: N2) => Nd4jUtil.gte(l.eval[V], r.eval[V])

      case And02(l: B0, r: B2) => Nd4jUtil.and(r.eval[INDArray], l.eval[Boolean])
      case And20(l: B2, r: B0) => Nd4jUtil.and(l.eval[INDArray], r.eval[Boolean])
      case And22(l: B2, r: B2) => Nd4jUtil.and(l.eval[INDArray], r.eval[INDArray])
      case Or02(l: B0, r: B2)  => Nd4jUtil.or(r.eval[INDArray], l.eval[Boolean])
      case Or20(l: B2, r: B0)  => Nd4jUtil.or(l.eval[INDArray], r.eval[Boolean])
      case Or22(l: B2, r: B2)  => Nd4jUtil.or(l.eval[INDArray], r.eval[INDArray])

      case Not2(v: B2) => v.eval[INDArray].map { e => if (e == 0.0) 1.0 else 0.0 }
    }

  }
}
