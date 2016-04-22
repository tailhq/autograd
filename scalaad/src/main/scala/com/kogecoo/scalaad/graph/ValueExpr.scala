package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.algorithm.{Eval, Forward, Grad, Reverse}
import com.kogecoo.scalaad.op.{Add00, Div00, Dot11, Eq00, Gt00, Gte00, Lt00, Lte00, MatMul22, Mul00, Neg0, Neq00, Pos0, Sub00}
import com.kogecoo.scalaad.{Shape, ShapeCheckException}

// g -> adjoint
// type dynamic http://seratch.hatenablog.jp/entry/2013/03/28/210928
// A.reverse(B) need to satisfy A.size == B.size when A and B are N1
trait ValueExpr[S <: Shape]  extends Expr[S]{
  val shape: S
  def forward[W, O](w: W)(implicit F: Forward[ValueExpr[S], W, O]): O = F.forward(this, w)
  def reverse[G](g: G)(implicit R: Reverse[ValueExpr[S], G]): Grad = R.reverse(this, g)

  def eval[V](implicit E: Eval[ValueExpr[S], V]): V = E.eval(this)
  def grad(implicit R: Reverse[ValueExpr[S], N0]): Grad = reverse[N0](One0())
}


object ValueExpr {

  implicit class Node0Op(val self: N0) extends AnyVal {
    def +(rhs: N0): N0 = Apply00(self, rhs, Add00)
    def -(rhs: N0): N0 = Apply00(self, rhs, Sub00)
    def *(rhs: N0): N0 = Apply00(self, rhs, Mul00)
    def /(rhs: N0): N0 = Apply00(self, rhs, Div00)

    def :+(rhs: N1): N1 = Broadcast01(self, rhs, Add00)
    def :-(rhs: N1): N1 = Broadcast01(self, rhs, Sub00)
    def :*(rhs: N1): N1 = Broadcast01(self, rhs, Mul00)
    def :/(rhs: N1): N1 = Broadcast01(self, rhs, Div00)

    def :+(rhs: N2): N2 = Broadcast02(self, rhs, Add00)
    def :-(rhs: N2): N2 = Broadcast02(self, rhs, Sub00)
    def :*(rhs: N2): N2 = Broadcast02(self, rhs, Mul00)
    def :/(rhs: N2): N2 = Broadcast02(self, rhs, Div00)

    def unary_+(): N0 = Apply0(self, Pos0)
    def unary_-(): N0 = Apply0(self, Neg0)

    def ==(rhs: N0): B0 = Eq00 (self, rhs)
    def !=(rhs: N0): B0 = Neq00(self, rhs)
    def < (rhs: N0): B0 = Lt00 (self, rhs)
    def <=(rhs: N0): B0 = Lte00(self, rhs)
    def > (rhs: N0): B0 = Gt00 (self, rhs)
    def >=(rhs: N0): B0 = Gte00(self, rhs)

    def :==(rhs: N1): B1 = Eq01 (self, rhs)
    def :!=(rhs: N1): B1 = Neq01(self, rhs)
    def :< (rhs: N1): B1 = Lt01 (self, rhs)
    def :<=(rhs: N1): B1 = Lte01(self, rhs)
    def :> (rhs: N1): B1 = Gt01 (self, rhs)
    def :>=(rhs: N1): B1 = Gte01(self, rhs)

    def :==(rhs: N2)(implicit d: DummyImplicit): B2 = Eq02 (self, rhs)
    def :!=(rhs: N2)(implicit d: DummyImplicit): B2 = Neq02(self, rhs)
    def :< (rhs: N2)(implicit d: DummyImplicit): B2 = Lt02 (self, rhs)
    def :<=(rhs: N2)(implicit d: DummyImplicit): B2 = Lte02(self, rhs)
    def :> (rhs: N2)(implicit d: DummyImplicit): B2 = Gt02 (self, rhs)
    def :>=(rhs: N2)(implicit d: DummyImplicit): B2 = Gte02(self, rhs)

  }

  implicit class Node1Op(val self: N1) extends AnyVal {

    private[this] def check(a: N1, op: String): Unit = {
      if (self.shape != a.shape) throw new ShapeCheckException(self, a, op)
    }
    private[this] def check(a: N2, op: String)(implicit d: DummyImplicit): Unit = {
      if (self.shape.transposed) {
        if (self.shape._1 != a.shape._2) throw new ShapeCheckException(self, a, op)
      } else {
        if (self.shape._1 != a.shape._1) throw new ShapeCheckException(self, a, op)
      }
    }

    def +(rhs: N1): N1 = { check(rhs, "Add11"); Elementwise11(self, rhs, Add00) }
    def -(rhs: N1): N1 = { check(rhs, "Sub11"); Elementwise11(self, rhs, Sub00) }
    def *(rhs: N1): N1 = { check(rhs, "Mul11"); Elementwise11(self, rhs, Mul00) }
    def /(rhs: N1): N1 = { check(rhs, "Div11"); Elementwise11(self, rhs, Div00) }
    def dot(rhs: N1): Dot11 = { check(rhs, Dot11.toString); Dot11(self, rhs) }

    def :+(rhs: N0): Broadcast10 = Broadcast10(self, rhs, Add00)
    def :-(rhs: N0): Broadcast10 = Broadcast10(self, rhs, Sub00)
    def :*(rhs: N0): Broadcast10 = Broadcast10(self, rhs, Mul00)
    def :/(rhs: N0): Broadcast10 = Broadcast10(self, rhs, Div00)

    def :+(rhs: N2)(implicit d: DummyImplicit): N2 = { check(rhs, "Add12"); Broadcast02(self, rhs, Add00) }
    def :-(rhs: N2)(implicit d: DummyImplicit): N2 = { check(rhs, "Sub12"); Broadcast02(self, rhs, Sub00) }
    def :*(rhs: N2)(implicit d: DummyImplicit): N2 = { check(rhs, "Mul12"); Broadcast02(self, rhs, Mul00) }
    def :/(rhs: N2)(implicit d: DummyImplicit): N2 = { check(rhs, "Div12"); Broadcast02(self, rhs, Div00) }

    def unary_+(): Elementwise1 = Elementwise1(self, Pos0)
    def unary_-(): Elementwise1 = Elementwise1(self, Neg0)
    def T: N1 = self match {
      case Transpose1(v) => v
      case v             => Transpose1(v)
    }

    def ==(rhs: N1): B1 = { check(rhs,  Eq11.toString); Eq11 (self, rhs) }
    def !=(rhs: N1): B1 = { check(rhs, Neq11.toString); Neq11(self, rhs) }
    def < (rhs: N1): B1 = { check(rhs,  Lt11.toString); Lt11 (self, rhs) }
    def <=(rhs: N1): B1 = { check(rhs, Lte11.toString); Lte11(self, rhs) }
    def > (rhs: N1): B1 = { check(rhs,  Gt11.toString); Gt11 (self, rhs) }
    def >=(rhs: N1): B1 = { check(rhs, Gte11.toString); Gte11(self, rhs) }

    def :==(rhs: N0): B1 = Eq10 (self, rhs)
    def :!=(rhs: N0): B1 = Neq10(self, rhs)
    def :< (rhs: N0): B1 = Lt10 (self, rhs)
    def :<=(rhs: N0): B1 = Lte10(self, rhs)
    def :> (rhs: N0): B1 = Gt10 (self, rhs)
    def :>=(rhs: N0): B1 = Gte10(self, rhs)

    //def :==(rhs: N2): B2 = Eq12 (self, rhs)
    //def :!=(rhs: N2): B2 = Neq12(self, rhs)
    //def :< (rhs: N2): B2 = Lt12 (self, rhs)
    //def :<=(rhs: N2): B2 = Lte12(self, rhs)
    //def :> (rhs: N2): B2 = Gt12 (self, rhs)
    //def :>=(rhs: N2): B2 = Gte12(self, rhs)

  }

  implicit class Node2Op(val self: N2) extends AnyVal {

    private[this] def check(rhs: N2, op: String): Unit = {
      if (self.shape != rhs.shape) throw new ShapeCheckException(self, rhs, op)
    }

    private[this] def check(rhs: N1, op: String)(implicit d: DummyImplicit): Unit = {
      if (rhs.shape.transposed) {
        if (self.shape._2 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
      } else {
        if (self.shape._1 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
      }
    }

    private[this] def matmulCheck(rhs: N2, op: String): Unit = {
      if (self.shape._2 != rhs.shape._1) throw new ShapeCheckException(self, rhs, op)
    }

    def +(rhs: N2): N2 = { check(rhs, "Add22"); Elementwise22(self, rhs, Add00) }
    def -(rhs: N2): N2 = { check(rhs, "Sub22"); Elementwise22(self, rhs, Sub00) }
    def *(rhs: N2): N2 = { check(rhs, "Mul22"); Elementwise22(self, rhs, Mul00) }
    def /(rhs: N2): N2 = { check(rhs, "Div22"); Elementwise22(self, rhs, Div00) }

    def :+(rhs: N0): N2 = Broadcast02(self, rhs, Add00)
    def :-(rhs: N0): N2 = Broadcast02(self, rhs, Sub00)
    def :*(rhs: N0): N2 = Broadcast02(self, rhs, Mul00)
    def :/(rhs: N0): N2 = Broadcast02(self, rhs, Div00)

    def :+(rhs: N1)(implicit d: DummyImplicit): N2 = { check(rhs, "Add21"); Broadcast12(self, rhs, Add00) }
    def :-(rhs: N1)(implicit d: DummyImplicit): N2 = { check(rhs, "Sub21"); Broadcast12(self, rhs, Sub00) }
    def :*(rhs: N1)(implicit d: DummyImplicit): N2 = { check(rhs, "Mul21"); Broadcast12(self, rhs, Mul00) }
    def :/(rhs: N1)(implicit d: DummyImplicit): N2 = { check(rhs, "Div21"); Broadcast12(self, rhs, Div00) }

    def matmul(rhs: N2): N2 = { matmulCheck(rhs, MatMul22.toString()); MatMul22(self, rhs) }

    def unary_+(): Pos2 = Pos2(self)
    def unary_-(): Neg2 = Neg2(self)
    def T: N2 = self match {
      case Transpose2(v) => v
      case v             => Transpose2(v)
    }

    def ==(rhs: N2): B2 = { check(rhs,  Eq22.toString);  Eq22(self, rhs) }
    def !=(rhs: N2): B2 = { check(rhs, Neq22.toString); Neq22(self, rhs) }
    def < (rhs: N2): B2 = { check(rhs,  Lt22.toString);  Lt22(self, rhs) }
    def <=(rhs: N2): B2 = { check(rhs, Lte22.toString); Lte22(self, rhs) }
    def > (rhs: N2): B2 = { check(rhs,  Gt22.toString);  Gt22(self, rhs) }
    def >=(rhs: N2): B2 = { check(rhs, Gte22.toString); Gte22(self, rhs) }

    def :==(rhs: N0): B2 = Eq20 (self, rhs)
    def :!=(rhs: N0): B2 = Neq20(self, rhs)
    def :< (rhs: N0): B2 = Lt20 (self, rhs)
    def :<=(rhs: N0): B2 = Lte20(self, rhs)
    def :> (rhs: N0): B2 = Gt20 (self, rhs)
    def :>=(rhs: N0): B2 = Gte20(self, rhs)

    //def :==(rhs: N1): B2 = Eq21 (self, rhs)
    //def :!=(rhs: N1): B2 = Neq21(self, rhs)
    //def :< (rhs: N1): B2 = Lt21 (self, rhs)
    //def :<=(rhs: N1): B2 = Lte21(self, rhs)
    //def :> (rhs: N1): B2 = Gt21 (self, rhs)
    //def :>=(rhs: N1): B2 = Gte21(self, rhs)

  }

}


