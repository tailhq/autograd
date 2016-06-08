package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.ShapeCheckException
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph.bool.{Apply2C, ElementwiseLeftC, ElementwiseRightC}
import com.kogecoo.scalaad.op.bool.{Eq, Gt, Gte, Lt, Lte, Neq}
import com.kogecoo.scalaad.op.{Add, Div, Mul, Neg, Pos, Sub}
import shapeless.Nat
import shapeless.ops.nat.LT.<
import shapeless.ops.nat.Sum



trait ValueExpr[N <: Nat]  extends Expr[N]{

  //def forward[W, O](w: W)(implicit F: Forward[ValueExpr[S], W, O]): O = F.forward(this, w)

  final def forward[W <: Nat, O <: Nat](wrt: ValueExpr[W])(implicit s: Sum.Aux[N, W, O]): ValueExpr[O] = {
    _forward[W, O](wrt)
  }

  //def reverse[G](g: G)(implicit R: Reverse[ValueExpr[N], G]): Grad = R.reverse(this, g)
  final def reverse[G <: Nat](g: ValueExpr[G]): Grad[G] = {
    val builder = new GradBuilder[G]
    _reverse(g, builder)
    builder.result()
  }

  def eval[R](implicit E: Eval[ValueExpr[N], R]): R = E.eval(this)

  //def grad(implicit R: Reverse[ValueExpr[N], VE0]): Grad = reverse[VE0](One0())

  def _forward[W <: Nat, O <: Nat](wrt: ValueExpr[W]): ValueExpr[O]

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit

}


object ValueExpr {

  implicit class RichValueExpr[N <: Nat](val self: V[N]) extends AnyVal {

    def +(rhs: V[N]): V[N] = Apply2(self, rhs, Add)
    def -(rhs: V[N]): V[N] = Apply2(self, rhs, Sub)
    def *(rhs: V[N]): V[N] = Apply2(self, rhs, Mul)
    def /(rhs: V[N]): V[N] = Apply2(self, rhs, Div)

    def +->[M <: Nat](rhs: V[M])(implicit ev: N < M): V[M] = ElementwiseRight(self, rhs, Add)
    def -->[M <: Nat](rhs: V[M])(implicit ev: N < M): V[M] = ElementwiseRight(self, rhs, Sub)
    def *->[M <: Nat](rhs: V[M])(implicit ev: N < M): V[M] = ElementwiseRight(self, rhs, Mul)
    def /->[M <: Nat](rhs: V[M])(implicit ev: N < M): V[M] = ElementwiseRight(self, rhs, Div)

    def <-+[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Add)
    def <--[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Sub)
    def <-*[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Mul)
    def <-/[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Div)

    def :+[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Add)
    def :-[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Sub)
    def :*[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Mul)
    def :/[M <: Nat](rhs: V[M])(implicit ev: M < N): V[N] = ElementwiseLeft(self, rhs, Div)

  }

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

    def ==(rhs: V0): BE0 = Apply2C(self, rhs, Eq)
    def !=(rhs: V0): BE0 = Apply2C(self, rhs, Neq)
    def < (rhs: V0): BE0 = Apply2C(self, rhs, Lt)
    def <=(rhs: V0): BE0 = Apply2C(self, rhs, Lte)
    def > (rhs: V0): BE0 = Apply2C(self, rhs, Gt)
    def >=(rhs: V0): BE0 = Apply2C(self, rhs, Gte)

    def :==(rhs: V1): BE1 = ElementwiseRightC(self, rhs, Eq)
    def :!=(rhs: V1): BE1 = ElementwiseRightC(self, rhs, Neq)
    def :< (rhs: V1): BE1 = ElementwiseRightC(self, rhs, Lt)
    def :<=(rhs: V1): BE1 = ElementwiseRightC(self, rhs, Lte)
    def :> (rhs: V1): BE1 = ElementwiseRightC(self, rhs, Gt)
    def :>=(rhs: V1): BE1 = ElementwiseRightC(self, rhs, Gte)

    def :==(rhs: V2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Eq)
    def :!=(rhs: V2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Neq)
    def :<(rhs: V2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Lt)
    def :<=(rhs: V2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Lte)
    def :>(rhs: V2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Gt)
    def :>=(rhs: V2)(implicit d: DummyImplicit): BE2 = ElementwiseRightC(self, rhs, Gte)

  }

  implicit class RichValueExpr1(val self: V1) extends AnyVal {

    // compiler crashes when prepend private[this]
    // https://issues.scala-lang.org/browse/SI-8847
    def check(a: V1, op: String): Unit = {
      if (self.shape != a.shape) throw new ShapeCheckException(self, a, op)
    }

    def check(a: V2, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape(0) != a.shape(0)) throw new ShapeCheckException(self, a, op)
    }

    def +(rhs: V1): V1 = { check(rhs, Add.toString); Apply2(self, rhs, Add) }
    def -(rhs: V1): V1 = { check(rhs, Sub.toString); Apply2(self, rhs, Sub) }
    def *(rhs: V1): V1 = { check(rhs, Mul.toString); Apply2(self, rhs, Mul) }
    def /(rhs: V1): V1 = { check(rhs, Div.toString); Apply2(self, rhs, Div) }
    //def dot(rhs: VE1): VE0 = { check(rhs, Dot.toString); Fold2(self, rhs, Dot) }

    def :+(rhs: V0): V1 = ElementwiseLeft(self, rhs, Add)
    def :-(rhs: V0): V1 = ElementwiseLeft(self, rhs, Sub)
    def :*(rhs: V0): V1 = ElementwiseLeft(self, rhs, Mul)
    def :/(rhs: V0): V1 = ElementwiseLeft(self, rhs, Div)

    def unary_+(): V1 = Apply1(self, Pos)
    def unary_-(): V1 = Apply1(self, Neg)

    def ==(rhs: V1): BE1 = { check(rhs, Eq. toString); Apply2C(self, rhs, Eq)  }
    def !=(rhs: V1): BE1 = { check(rhs, Neq.toString); Apply2C(self, rhs, Neq) }
    def < (rhs: V1): BE1 = { check(rhs, Lt. toString); Apply2C(self, rhs, Lt)  }
    def <=(rhs: V1): BE1 = { check(rhs, Lte.toString); Apply2C(self, rhs, Lte) }
    def > (rhs: V1): BE1 = { check(rhs, Gt. toString); Apply2C(self, rhs, Gt)  }
    def >=(rhs: V1): BE1 = { check(rhs, Gte.toString); Apply2C(self, rhs, Gte) }

    def :==(rhs: V0): BE1 = ElementwiseLeftC(self, rhs, Eq)
    def :!=(rhs: V0): BE1 = ElementwiseLeftC(self, rhs, Neq)
    def :< (rhs: V0): BE1 = ElementwiseLeftC(self, rhs, Lt)
    def :<=(rhs: V0): BE1 = ElementwiseLeftC(self, rhs, Lte)
    def :> (rhs: V0): BE1 = ElementwiseLeftC(self, rhs, Gt)
    def :>=(rhs: V0): BE1 = ElementwiseLeftC(self, rhs, Gte)

  }

  implicit class RichValueExpr2(val self: V2) extends AnyVal {

    def check(rhs: V2, op: String): Unit = {
      if (self.shape != rhs.shape) throw new ShapeCheckException(self, rhs, op)
    }

    def check(rhs: V1, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape(0) != rhs.shape(0)) throw new ShapeCheckException(self, rhs, op)
    }

    private[this] def matmulCheck(rhs: V2, op: String): Unit = {
      if (self.shape(1) != rhs.shape(0)) throw new ShapeCheckException(self, rhs, op)
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

    def ==(rhs: V2): BE2 = { check(rhs, "Eq22" ); Apply2C(self, rhs, Eq)  }
    def !=(rhs: V2): BE2 = { check(rhs, "Neq22"); Apply2C(self, rhs, Neq) }
    def < (rhs: V2): BE2 = { check(rhs, "Lt22" ); Apply2C(self, rhs, Lt)  }
    def <=(rhs: V2): BE2 = { check(rhs, "Lte22"); Apply2C(self, rhs, Lte) }
    def > (rhs: V2): BE2 = { check(rhs, "Gt22" ); Apply2C(self, rhs, Gt)  }
    def >=(rhs: V2): BE2 = { check(rhs, "Gte22"); Apply2C(self, rhs, Gte) }

    def :==(rhs: V0): BE2 = ElementwiseLeftC(self, rhs, Eq)
    def :!=(rhs: V0): BE2 = ElementwiseLeftC(self, rhs, Neq)
    def :< (rhs: V0): BE2 = ElementwiseLeftC(self, rhs, Lt)
    def :<=(rhs: V0): BE2 = ElementwiseLeftC(self, rhs, Lte)
    def :> (rhs: V0): BE2 = ElementwiseLeftC(self, rhs, Gt)
    def :>=(rhs: V0): BE2 = ElementwiseLeftC(self, rhs, Gte)

  }

}


