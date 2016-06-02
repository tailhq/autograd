package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.algorithm.{Eval, Forward, Grad, Reverse}
import com.kogecoo.scalaad.analyze.{Analyzed, Analyzing, Param}
import com.kogecoo.scalaad.graph.bool.{Apply2C, ElementwiseLeftC, ElementwiseRightC}
import com.kogecoo.scalaad.op.bool.{Eq, Gt, Gte, Lt, Lte, Neq}
import com.kogecoo.scalaad.op.{Add, Div, Dot, Mul, Neg, Operator, Pos, Sub}
import com.kogecoo.scalaad.{Shape, ShapeCheckException}


trait ValueExpr[S <: Shape]  extends Expr[S]{

  def analyze(): Analyzed = {
    val analyzing = new Analyzing()
    this.analyze(analyzing)
    analyzing.result()
  }

  def analyze(analyzing: Analyzing): Param[S]

  //def forward[W, O](w: W)(implicit F: Forward[ValueExpr[S], W, O]): O = F.forward(this, w)
  def forward[SW, SO](w: ValueExpr[SW])(implicit F: Forward[ValueExpr[S], SW, SO]): ValueExpr[SO] = F.forward(this, w)

  def reverse[G](g: G)(implicit R: Reverse[ValueExpr[S], G]): Grad = R.reverse(this, g)

  def eval[R](implicit E: Eval[ValueExpr[S], R]): R = E.eval(this)

  def grad(implicit R: Reverse[ValueExpr[S], VE0]): Grad = reverse[VE0](One0())

}


object ValueExpr {

  implicit class RichValueExpr0(val self: VE0) extends AnyVal {

    def +(rhs: VE0): VE0 = Apply2(self, rhs, Add)
    def -(rhs: VE0): VE0 = Apply2(self, rhs, Sub)
    def *(rhs: VE0): VE0 = Apply2(self, rhs, Mul)
    def /(rhs: VE0): VE0 = Apply2(self, rhs, Div)

    def :+(rhs: VE1): VE1 = ElementwiseRight(self, rhs, Add)
    def :-(rhs: VE1): VE1 = ElementwiseRight(self, rhs, Sub)
    def :*(rhs: VE1): VE1 = ElementwiseRight(self, rhs, Mul)
    def :/(rhs: VE1): VE1 = ElementwiseRight(self, rhs, Div)

    def :+(rhs: VE2)(implicit d: DummyImplicit): VE2 = ElementwiseRight(self, rhs, Add)
    def :-(rhs: VE2)(implicit d: DummyImplicit): VE2 = ElementwiseRight(self, rhs, Sub)
    def :*(rhs: VE2)(implicit d: DummyImplicit): VE2 = ElementwiseRight(self, rhs, Mul)
    def :/(rhs: VE2)(implicit d: DummyImplicit): VE2 = ElementwiseRight(self, rhs, Div)

    def unary_+(): VE0 = Apply1(self, Pos)
    def unary_-(): VE0 = Apply1(self, Neg)

    def ==(rhs: VE0): BE0 = Apply2C(self, rhs, Eq)
    def !=(rhs: VE0): BE0 = Apply2C(self, rhs, Neq)
    def < (rhs: VE0): BE0 = Apply2C(self, rhs, Lt)
    def <=(rhs: VE0): BE0 = Apply2C(self, rhs, Lte)
    def > (rhs: VE0): BE0 = Apply2C(self, rhs, Gt)
    def >=(rhs: VE0): BE0 = Apply2C(self, rhs, Gte)

    def :==(rhs: VE1): BE1 = ElementwiseRightC(self, rhs, Eq)
    def :!=(rhs: VE1): BE1 = ElementwiseRightC(self, rhs, Neq)
    def :< (rhs: VE1): BE1 = ElementwiseRightC(self, rhs, Lt)
    def :<=(rhs: VE1): BE1 = ElementwiseRightC(self, rhs, Lte)
    def :> (rhs: VE1): BE1 = ElementwiseRightC(self, rhs, Gt)
    def :>=(rhs: VE1): BE1 = ElementwiseRightC(self, rhs, Gte)

    def :==(rhs: VE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Eq)
    def :!=(rhs: VE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Neq)
    def :<(rhs: VE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Lt)
    def :<=(rhs: VE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Lte)
    def :>(rhs: VE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Gt)
    def :>=(rhs: VE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Gte)

  }

  implicit class RichValueExpr1(val self: VE1) extends AnyVal {

    // compiler crashes when prepend private[this]
    // https://issues.scala-lang.org/browse/SI-8847
    def check(a: VE1, op: String): Unit = {
      if (self.shape != a.shape) throw new ShapeCheckException(self, a, op)
    }

    def check(a: VE2, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape._1 != a.shape._1) throw new ShapeCheckException(self, a, op)
    }

    def +(rhs: VE1): VE1 = { check(rhs, Add.toString); Apply2(self, rhs, Add) }
    def -(rhs: VE1): VE1 = { check(rhs, Sub.toString); Apply2(self, rhs, Sub) }
    def *(rhs: VE1): VE1 = { check(rhs, Mul.toString); Apply2(self, rhs, Mul) }
    def /(rhs: VE1): VE1 = { check(rhs, Div.toString); Apply2(self, rhs, Div) }
    def dot(rhs: VE1): VE0 = { check(rhs, Dot.toString); Fold2(self, rhs, Dot) }

    def :+(rhs: VE0): VE1 = ElementwiseLeft(self, rhs, Add)
    def :-(rhs: VE0): VE1 = ElementwiseLeft(self, rhs, Sub)
    def :*(rhs: VE0): VE1 = ElementwiseLeft(self, rhs, Mul)
    def :/(rhs: VE0): VE1 = ElementwiseLeft(self, rhs, Div)

    def unary_+(): VE1 = Apply1(self, Pos)
    def unary_-(): VE1 = Apply1(self, Neg)

    def ==(rhs: VE1): BE1 = { check(rhs, Eq. toString); Apply2C(self, rhs, Eq)  }
    def !=(rhs: VE1): BE1 = { check(rhs, Neq.toString); Apply2C(self, rhs, Neq) }
    def < (rhs: VE1): BE1 = { check(rhs, Lt. toString); Apply2C(self, rhs, Lt)  }
    def <=(rhs: VE1): BE1 = { check(rhs, Lte.toString); Apply2C(self, rhs, Lte) }
    def > (rhs: VE1): BE1 = { check(rhs, Gt. toString); Apply2C(self, rhs, Gt)  }
    def >=(rhs: VE1): BE1 = { check(rhs, Gte.toString); Apply2C(self, rhs, Gte) }

    def :==(rhs: VE0): BE1 = ElementwiseLeftC(self, rhs, Eq)
    def :!=(rhs: VE0): BE1 = ElementwiseLeftC(self, rhs, Neq)
    def :< (rhs: VE0): BE1 = ElementwiseLeftC(self, rhs, Lt)
    def :<=(rhs: VE0): BE1 = ElementwiseLeftC(self, rhs, Lte)
    def :> (rhs: VE0): BE1 = ElementwiseLeftC(self, rhs, Gt)
    def :>=(rhs: VE0): BE1 = ElementwiseLeftC(self, rhs, Gte)

  }

  implicit class RichValueExpr2(val self: VE2) extends AnyVal {

    def check(rhs: VE2, op: String): Unit = {
      if (self.shape != rhs.shape) throw new ShapeCheckException(self, rhs, op)
    }

    def check(rhs: VE1, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape._1 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
    }

    private[this] def matmulCheck(rhs: VE2, op: String): Unit = {
      if (self.shape._2 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
    }

    def +(rhs: VE2): VE2 = { check(rhs, "Add22"); Apply2(self, rhs, Add) }
    def -(rhs: VE2): VE2 = { check(rhs, "Sub22"); Apply2(self, rhs, Sub) }
    def *(rhs: VE2): VE2 = { check(rhs, "Mul22"); Apply2(self, rhs, Mul) }
    def /(rhs: VE2): VE2 = { check(rhs, "Div22"); Apply2(self, rhs, Div) }

    def :+(rhs: VE0): VE2 = ElementwiseLeft(self, rhs, Add)
    def :-(rhs: VE0): VE2 = ElementwiseLeft(self, rhs, Sub)
    def :*(rhs: VE0): VE2 = ElementwiseLeft(self, rhs, Mul)
    def :/(rhs: VE0): VE2 = ElementwiseLeft(self, rhs, Div)

    //def matmul(rhs: V2): V2 = { matmulCheck(rhs, MatMul22.toString()); MatMul22(self, rhs) }

    def unary_+(): VE2 = Apply1(self, Pos)
    def unary_-(): VE2 = Apply1(self, Neg)

    def ==(rhs: VE2): BE2 = { check(rhs, "Eq22" ); Apply2C(self, rhs, Eq)  }
    def !=(rhs: VE2): BE2 = { check(rhs, "Neq22"); Apply2C(self, rhs, Neq) }
    def < (rhs: VE2): BE2 = { check(rhs, "Lt22" ); Apply2C(self, rhs, Lt)  }
    def <=(rhs: VE2): BE2 = { check(rhs, "Lte22"); Apply2C(self, rhs, Lte) }
    def > (rhs: VE2): BE2 = { check(rhs, "Gt22" ); Apply2C(self, rhs, Gt)  }
    def >=(rhs: VE2): BE2 = { check(rhs, "Gte22"); Apply2C(self, rhs, Gte) }

    def :==(rhs: VE0): BE2 = ElementwiseLeftC(self, rhs, Eq)
    def :!=(rhs: VE0): BE2 = ElementwiseLeftC(self, rhs, Neq)
    def :< (rhs: VE0): BE2 = ElementwiseLeftC(self, rhs, Lt)
    def :<=(rhs: VE0): BE2 = ElementwiseLeftC(self, rhs, Lte)
    def :> (rhs: VE0): BE2 = ElementwiseLeftC(self, rhs, Gt)
    def :>=(rhs: VE0): BE2 = ElementwiseLeftC(self, rhs, Gte)

  }

}


