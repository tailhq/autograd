package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.algorithm.{Eval, Forward, Grad, Reverse}
import com.kogecoo.scalaad.op.{Add00, Div00, Dot11, Eq00, Gt00, Gte00, Lt00, Lte00, MatMul22, Mul00, Neg0, Neq00, Pos0, Sub00}
import com.kogecoo.scalaad.{Shape, ShapeCheckException}


trait ValueExpr[S <: Shape]  extends Expr[S]{

  val shape: S

  def forward[W, O](w: W)(implicit F: Forward[ValueExpr[S], W, O]): O = F.forward(this, w)

  def reverse[G](g: G)(implicit R: Reverse[ValueExpr[S], G]): Grad = R.reverse(this, g)

  def eval[V](implicit E: Eval[ValueExpr[S], V]): V = E.eval(this)

  def grad(implicit R: Reverse[ValueExpr[S], V0]): Grad = reverse[V0](One0())

}


object ValueExpr {

  implicit class RichValueExpr0(val self: V0) extends AnyVal {

    def +(rhs: V0): V0 = Apply00(self, rhs, Add00)
    def -(rhs: V0): V0 = Apply00(self, rhs, Sub00)
    def *(rhs: V0): V0 = Apply00(self, rhs, Mul00)
    def /(rhs: V0): V0 = Apply00(self, rhs, Div00)

    def :+(rhs: V1): V1 = Broadcast01(self, rhs, Add00)
    def :-(rhs: V1): V1 = Broadcast01(self, rhs, Sub00)
    def :*(rhs: V1): V1 = Broadcast01(self, rhs, Mul00)
    def :/(rhs: V1): V1 = Broadcast01(self, rhs, Div00)

    def :+(rhs: V2): V2 = Broadcast02(self, rhs, Add00)
    def :-(rhs: V2): V2 = Broadcast02(self, rhs, Sub00)
    def :*(rhs: V2): V2 = Broadcast02(self, rhs, Mul00)
    def :/(rhs: V2): V2 = Broadcast02(self, rhs, Div00)

    def unary_+(): V0 = Apply0(self, Pos0)
    def unary_-(): V0 = Apply0(self, Neg0)

    def ==(rhs: V0): B0 = Apply00(self, rhs, Eq00)
    def !=(rhs: V0): B0 = Apply00(self, rhs, Neq00)
    def < (rhs: V0): B0 = Apply00(self, rhs, Lt00)
    def <=(rhs: V0): B0 = Apply00(self, rhs, Lte00)
    def > (rhs: V0): B0 = Apply00(self, rhs, Gt00)
    def >=(rhs: V0): B0 = Apply00(self, rhs, Gte00)

    def :==(rhs: V1): B1 = Broadcast01(self, rhs, Eq00)
    def :!=(rhs: V1): B1 = Broadcast01(self, rhs, Neq00)
    def :< (rhs: V1): B1 = Broadcast01(self, rhs, Lt00)
    def :<=(rhs: V1): B1 = Broadcast01(self, rhs, Lte00)
    def :> (rhs: V1): B1 = Broadcast01(self, rhs, Gt00)
    def :>=(rhs: V1): B1 = Broadcast01(self, rhs, Gte00)

    def :==(rhs: V2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Eq00)
    def :!=(rhs: V2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Neq00)
    def :< (rhs: V2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Lt00)
    def :<=(rhs: V2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Lte00)
    def :> (rhs: V2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Gt00)
    def :>=(rhs: V2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Gte00)

  }

  implicit class RichValueExpr1(val self: V1) extends AnyVal {

    private[this] def check(a: V1, op: String): Unit = {
      if (self.shape != a.shape) throw new ShapeCheckException(self, a, op)
    }
    private[this] def check(a: V2, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape.transposed) {
        if (self.shape._1 != a.shape._2) throw new ShapeCheckException(self, a, op)
      } else {
        if (self.shape._1 != a.shape._1) throw new ShapeCheckException(self, a, op)
      }
    }

    def +(rhs: V1): V1 = { check(rhs, "Add11"); Elementwise11(self, rhs, Add00) }
    def -(rhs: V1): V1 = { check(rhs, "Sub11"); Elementwise11(self, rhs, Sub00) }
    def *(rhs: V1): V1 = { check(rhs, "Mul11"); Elementwise11(self, rhs, Mul00) }
    def /(rhs: V1): V1 = { check(rhs, "Div11"); Elementwise11(self, rhs, Div00) }
    def dot(rhs: V1): Dot11 = { check(rhs, Dot11.toString); Dot11(self, rhs) }

    def :+(rhs: V0): Broadcast10 = Broadcast10(self, rhs, Add00)
    def :-(rhs: V0): Broadcast10 = Broadcast10(self, rhs, Sub00)
    def :*(rhs: V0): Broadcast10 = Broadcast10(self, rhs, Mul00)
    def :/(rhs: V0): Broadcast10 = Broadcast10(self, rhs, Div00)

    def :+(rhs: V2)(implicit d: DummyImplicit): V2 = { check(rhs, "Add12"); Broadcast02(self, rhs, Add00) }
    def :-(rhs: V2)(implicit d: DummyImplicit): V2 = { check(rhs, "Sub12"); Broadcast02(self, rhs, Sub00) }
    def :*(rhs: V2)(implicit d: DummyImplicit): V2 = { check(rhs, "Mul12"); Broadcast02(self, rhs, Mul00) }
    def :/(rhs: V2)(implicit d: DummyImplicit): V2 = { check(rhs, "Div12"); Broadcast02(self, rhs, Div00) }

    def unary_+(): Elementwise1 = Elementwise1(self, Pos0)
    def unary_-(): Elementwise1 = Elementwise1(self, Neg0)
    def T: V1 = self match {
      case Transpose1(v) => v
      case v             => Transpose1(v)
    }

    def ==(rhs: V1): B1 = { check(rhs, "Eq11" ); Elementwise11(self, rhs, Eq00)  }
    def !=(rhs: V1): B1 = { check(rhs, "Neq11"); Elementwise11(self, rhs, Neq00) }
    def < (rhs: V1): B1 = { check(rhs, "Lt11" ); Elementwise11(self, rhs, Lt00)  }
    def <=(rhs: V1): B1 = { check(rhs, "Lte11"); Elementwise11(self, rhs, Lte00) }
    def > (rhs: V1): B1 = { check(rhs, "Gt11" ); Elementwise11(self, rhs, Gt00)  }
    def >=(rhs: V1): B1 = { check(rhs, "Gte11"); Elementwise11(self, rhs, Gte00) }

    def :==(rhs: V0): B1 = Broadcast10(self, rhs, Eq00 )
    def :!=(rhs: V0): B1 = Broadcast10(self, rhs, Neq00)
    def :< (rhs: V0): B1 = Broadcast10(self, rhs, Lt00 )
    def :<=(rhs: V0): B1 = Broadcast10(self, rhs, Lte00)
    def :> (rhs: V0): B1 = Broadcast10(self, rhs, Gt00 )
    def :>=(rhs: V0): B1 = Broadcast10(self, rhs, Gte00)

    //def :==(rhs: N2): B2 = Eq12 (self, rhs)
    //def :!=(rhs: N2): B2 = Neq12(self, rhs)
    //def :< (rhs: N2): B2 = Lt12 (self, rhs)
    //def :<=(rhs: N2): B2 = Lte12(self, rhs)
    //def :> (rhs: N2): B2 = Gt12 (self, rhs)
    //def :>=(rhs: N2): B2 = Gte12(self, rhs)

  }

  implicit class RichValueExpr2(val self: V2) extends AnyVal {

    private[this] def check(rhs: V2, op: String): Unit = {
      if (self.shape != rhs.shape) throw new ShapeCheckException(self, rhs, op)
    }

    private[this] def check(rhs: V1, op: String)(implicit d: DummyImplicit): Unit = {
      if (rhs.shape.transposed) {
        if (self.shape._2 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
      } else {
        if (self.shape._1 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
      }
    }

    private[this] def matmulCheck(rhs: V2, op: String): Unit = {
      if (self.shape._2 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
    }

    def +(rhs: V2): V2 = { check(rhs, "Add22"); Elementwise22(self, rhs, Add00) }
    def -(rhs: V2): V2 = { check(rhs, "Sub22"); Elementwise22(self, rhs, Sub00) }
    def *(rhs: V2): V2 = { check(rhs, "Mul22"); Elementwise22(self, rhs, Mul00) }
    def /(rhs: V2): V2 = { check(rhs, "Div22"); Elementwise22(self, rhs, Div00) }

    def :+(rhs: V0): V2 = Broadcast02(self, rhs, Add00)
    def :-(rhs: V0): V2 = Broadcast02(self, rhs, Sub00)
    def :*(rhs: V0): V2 = Broadcast02(self, rhs, Mul00)
    def :/(rhs: V0): V2 = Broadcast02(self, rhs, Div00)

    def :+(rhs: V1)(implicit d: DummyImplicit): V2 = { check(rhs, "Add21"); Broadcast12(self, rhs, Add00) }
    def :-(rhs: V1)(implicit d: DummyImplicit): V2 = { check(rhs, "Sub21"); Broadcast12(self, rhs, Sub00) }
    def :*(rhs: V1)(implicit d: DummyImplicit): V2 = { check(rhs, "Mul21"); Broadcast12(self, rhs, Mul00) }
    def :/(rhs: V1)(implicit d: DummyImplicit): V2 = { check(rhs, "Div21"); Broadcast12(self, rhs, Div00) }

    def matmul(rhs: V2): V2 = { matmulCheck(rhs, MatMul22.toString()); MatMul22(self, rhs) }

    def unary_+(): V2 = Elementwise2(self, Pos0)
    def unary_-(): V2 = Elementwise2(self, Neg0)
    def T: V2 = self match {
      case Transpose2(v) => v
      case v             => Transpose2(v)
    }

    def ==(rhs: V2): B2 = { check(rhs, "Eq22" ); Elementwise22(self, rhs, Eq00)  }
    def !=(rhs: V2): B2 = { check(rhs, "Neq22"); Elementwise22(self, rhs, Neq00) }
    def < (rhs: V2): B2 = { check(rhs, "Lt22" ); Elementwise22(self, rhs, Lt00)  }
    def <=(rhs: V2): B2 = { check(rhs, "Lte22"); Elementwise22(self, rhs, Lte00) }
    def > (rhs: V2): B2 = { check(rhs, "Gt22" ); Elementwise22(self, rhs, Gt00)  }
    def >=(rhs: V2): B2 = { check(rhs, "Gte22"); Elementwise22(self, rhs, Gte00) }

    def :==(rhs: V0): B2 = Broadcast02(self, rhs, Eq00)
    def :!=(rhs: V0): B2 = Broadcast02(self, rhs, Neq00)
    def :< (rhs: V0): B2 = Broadcast02(self, rhs, Lt00)
    def :<=(rhs: V0): B2 = Broadcast02(self, rhs, Lte00)
    def :> (rhs: V0): B2 = Broadcast02(self, rhs, Gt00)
    def :>=(rhs: V0): B2 = Broadcast02(self, rhs, Gte00)

    //def :==(rhs: N1): B2 = Eq21 (self, rhs)
    //def :!=(rhs: N1): B2 = Neq21(self, rhs)
    //def :< (rhs: N1): B2 = Lt21 (self, rhs)
    //def :<=(rhs: N1): B2 = Lte21(self, rhs)
    //def :> (rhs: N1): B2 = Gt21 (self, rhs)
    //def :>=(rhs: N1): B2 = Gte21(self, rhs)

  }

}


