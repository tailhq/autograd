package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.algorithm.{Analyze, Eval, Forward, Grad, Reverse}
import com.kogecoo.scalaad.analyze.{Analyzed, Analyzing, Equation, Param}
import com.kogecoo.scalaad.op.{Add, Div, Dot, Eq, Gt, Gte, Lt, Lte, Mul, Neg, Neq, Pos, Sub}
import com.kogecoo.scalaad.{S0, Shape, ShapeCheckException}


trait ValueExpr[S <: Shape]  extends Expr[S]{

  def analyze()(implicit A: Analyze[S]): Analyzed = {
    val analyzing = new Analyzing()
    this.analyze(analyzing)
    analyzing.result()
  }

  def analyze(analyzing: Analyzing)(implicit A: Analyze[S]): Param[S] = {
    A.analyze(this, analyzing)
  }

  def forward[W, O](w: W)(implicit F: Forward[ValueExpr[S], W, O]): O = F.forward(this, w)

  def reverse[G](g: G)(implicit R: Reverse[ValueExpr[S], G]): Grad = R.reverse(this, g)

  def eval[V](implicit E: Eval[ValueExpr[S], V]): V = E.eval(this)

  def grad(implicit R: Reverse[ValueExpr[S], V0]): Grad = reverse[V0](One0())

}


object ValueExpr {

  implicit class RichValueExpr0(val self: V0) extends AnyVal {

    def +(rhs: V0): V0 = Apply2(self, rhs, Add)
    def -(rhs: V0): V0 = Apply2(self, rhs, Sub)
    def *(rhs: V0): V0 = Apply2(self, rhs, Mul)
    def /(rhs: V0): V0 = Apply2(self, rhs, Div)

    def :+(rhs: V1): V1 = ElementwiseRight(self, rhs, Add)
    def :-(rhs: V1): V1 = ElementwiseRight(self, rhs, Sub)
    def :*(rhs: V1): V1 = ElementwiseRight(self, rhs, Mul)
    def :/(rhs: V1): V1 = ElementwiseRight(self, rhs, Div)

    def :+(rhs: V2)(implicit d: DummyImplicit): V2 = ElementwiseRight(self, rhs, Add)
    def :-(rhs: V2)(implicit d: DummyImplicit): V2 = ElementwiseRight(self, rhs, Sub)
    def :*(rhs: V2)(implicit d: DummyImplicit): V2 = ElementwiseRight(self, rhs, Mul)
    def :/(rhs: V2)(implicit d: DummyImplicit): V2 = ElementwiseRight(self, rhs, Div)

    def unary_+(): V0 = Apply1(self, Pos)
    def unary_-(): V0 = Apply1(self, Neg)

    def ==(rhs: V0): B0 = Apply00C(self, rhs, Eq)
    def !=(rhs: V0): B0 = Apply00C(self, rhs, Neq)
    def < (rhs: V0): B0 = Apply00C(self, rhs, Lt)
    def <=(rhs: V0): B0 = Apply00C(self, rhs, Lte)
    def > (rhs: V0): B0 = Apply00C(self, rhs, Gt)
    def >=(rhs: V0): B0 = Apply00C(self, rhs, Gte)

    def :==(rhs: V1): B1 = Elementwise01C(self, rhs, Eq)
    def :!=(rhs: V1): B1 = Elementwise01C(self, rhs, Neq)
    def :< (rhs: V1): B1 = Elementwise01C(self, rhs, Lt)
    def :<=(rhs: V1): B1 = Elementwise01C(self, rhs, Lte)
    def :> (rhs: V1): B1 = Elementwise01C(self, rhs, Gt)
    def :>=(rhs: V1): B1 = Elementwise01C(self, rhs, Gte)

    def :==(rhs: V2)(implicit d: DummyImplicit): B2 = Elementwise02C(self, rhs, Eq)
    def :!=(rhs: V2)(implicit d: DummyImplicit): B2 = Elementwise02C(self, rhs, Neq)
    def :< (rhs: V2)(implicit d: DummyImplicit): B2 = Elementwise02C(self, rhs, Lt)
    def :<=(rhs: V2)(implicit d: DummyImplicit): B2 = Elementwise02C(self, rhs, Lte)
    def :> (rhs: V2)(implicit d: DummyImplicit): B2 = Elementwise02C(self, rhs, Gt)
    def :>=(rhs: V2)(implicit d: DummyImplicit): B2 = Elementwise02C(self, rhs, Gte)

  }

  implicit class RichValueExpr1(val self: V1) extends AnyVal {

    // compiler crashes when prepend private[this]
    // https://issues.scala-lang.org/browse/SI-8847
    def check(a: V1, op: String): Unit = {
      if (self.shape != a.shape) throw new ShapeCheckException(self, a, op)
    }

    def check(a: V2, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape._1 != a.shape._1) throw new ShapeCheckException(self, a, op)
    }

    def +(rhs: V1): V1 = { check(rhs, "Add11"); Apply2(self, rhs, Add) }
    def -(rhs: V1): V1 = { check(rhs, "Sub11"); Apply2(self, rhs, Sub) }
    def *(rhs: V1): V1 = { check(rhs, "Mul11"); Apply2(self, rhs, Mul) }
    def /(rhs: V1): V1 = { check(rhs, "Div11"); Apply2(self, rhs, Div) }
    def dot(rhs: V1): V0 = { check(rhs, "Dot11"); Fold2(self, rhs, Dot) }

    def :+(rhs: V0): V1 = ElementwiseLeft(self, rhs, Add)
    def :-(rhs: V0): V1 = ElementwiseLeft(self, rhs, Sub)
    def :*(rhs: V0): V1 = ElementwiseLeft(self, rhs, Mul)
    def :/(rhs: V0): V1 = ElementwiseLeft(self, rhs, Div)

    def unary_+(): V1 = Apply1(self, Pos)
    def unary_-(): V1 = Apply1(self, Neg)

    def ==(rhs: V1): B1 = { check(rhs, "Eq11" ); Elementwise11C(self, rhs, Eq)  }
    def !=(rhs: V1): B1 = { check(rhs, "Neq11"); Elementwise11C(self, rhs, Neq) }
    def < (rhs: V1): B1 = { check(rhs, "Lt11" ); Elementwise11C(self, rhs, Lt)  }
    def <=(rhs: V1): B1 = { check(rhs, "Lte11"); Elementwise11C(self, rhs, Lte) }
    def > (rhs: V1): B1 = { check(rhs, "Gt11" ); Elementwise11C(self, rhs, Gt)  }
    def >=(rhs: V1): B1 = { check(rhs, "Gte11"); Elementwise11C(self, rhs, Gte) }

    def :==(rhs: V0): B1 = Elementwise10C(self, rhs, Eq)
    def :!=(rhs: V0): B1 = Elementwise10C(self, rhs, Neq)
    def :< (rhs: V0): B1 = Elementwise10C(self, rhs, Lt)
    def :<=(rhs: V0): B1 = Elementwise10C(self, rhs, Lte)
    def :> (rhs: V0): B1 = Elementwise10C(self, rhs, Gt)
    def :>=(rhs: V0): B1 = Elementwise10C(self, rhs, Gte)

  }

  implicit class RichValueExpr2(val self: V2) extends AnyVal {

    def check(rhs: V2, op: String): Unit = {
      if (self.shape != rhs.shape) throw new ShapeCheckException(self, rhs, op)
    }

    def check(rhs: V1, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape._1 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
    }

    private[this] def matmulCheck(rhs: V2, op: String): Unit = {
      if (self.shape._2 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
    }

    def +(rhs: V2): V2 = { check(rhs, "Add22"); Apply2(self, rhs, Add) }
    def -(rhs: V2): V2 = { check(rhs, "Sub22"); Apply2(self, rhs, Sub) }
    def *(rhs: V2): V2 = { check(rhs, "Mul22"); Apply2(self, rhs, Mul) }
    def /(rhs: V2): V2 = { check(rhs, "Div22"); Apply2(self, rhs, Div) }

    def :+(rhs: V0): V2 = ElementwiseLeft(self, rhs, Add)
    def :-(rhs: V0): V2 = ElementwiseLeft(self, rhs, Sub)
    def :*(rhs: V0): V2 = ElementwiseLeft(self, rhs, Mul)
    def :/(rhs: V0): V2 = ElementwiseLeft(self, rhs, Div)

    //def matmul(rhs: V2): V2 = { matmulCheck(rhs, MatMul22.toString()); MatMul22(self, rhs) }

    def unary_+(): V2 = Apply1(self, Pos)
    def unary_-(): V2 = Apply1(self, Neg)

    def ==(rhs: V2): B2 = { check(rhs, "Eq22" ); Elementwise22C(self, rhs, Eq)  }
    def !=(rhs: V2): B2 = { check(rhs, "Neq22"); Elementwise22C(self, rhs, Neq) }
    def < (rhs: V2): B2 = { check(rhs, "Lt22" ); Elementwise22C(self, rhs, Lt)  }
    def <=(rhs: V2): B2 = { check(rhs, "Lte22"); Elementwise22C(self, rhs, Lte) }
    def > (rhs: V2): B2 = { check(rhs, "Gt22" ); Elementwise22C(self, rhs, Gt)  }
    def >=(rhs: V2): B2 = { check(rhs, "Gte22"); Elementwise22C(self, rhs, Gte) }

    def :==(rhs: V0): B2 = Elementwise20C(self, rhs, Eq)
    def :!=(rhs: V0): B2 = Elementwise20C(self, rhs, Neq)
    def :< (rhs: V0): B2 = Elementwise20C(self, rhs, Lt)
    def :<=(rhs: V0): B2 = Elementwise20C(self, rhs, Lte)
    def :> (rhs: V0): B2 = Elementwise20C(self, rhs, Gt)
    def :>=(rhs: V0): B2 = Elementwise20C(self, rhs, Gte)

  }

}


