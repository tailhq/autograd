package com.kogecoo.scalaad.graph


import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.algorithm.{Grad, Eval, Forward, Reverse}

// g -> adjoint
// type dynamic http://seratch.hatenablog.jp/entry/2013/03/28/210928
// A.reverse(B) need to satisfy A.size == B.size when A and B are N1
trait Node[S <: Shape] {
  val shape: S
  def forward[W, O](w: W)(implicit F: Forward[Node[S], W, O]): O = F.forward(this, w)
  def reverse[G](g: G)(implicit R: Reverse[Node[S], G]): Grad = R.reverse(this, g)

  def eval[V](implicit E: Eval[Node[S], V]): V = E.eval(this)
  def grad(implicit R: Reverse[Node[S], N0]): Grad = reverse[N0](One0())
}


object Node {

  implicit class Node0Op(val self: N0) extends AnyVal {
    def +(rhs: N0): Add00 = Add00(self, rhs)
    def -(rhs: N0): Sub00 = Sub00(self, rhs)
    def *(rhs: N0): Mul00 = Mul00(self, rhs)
    def /(rhs: N0): Div00 = Div00(self, rhs)

    def :+(rhs: N1): Add01 = Add01(self, rhs)
    def :-(rhs: N1): Sub01 = Sub01(self, rhs)
    def :*(rhs: N1): Mul01 = Mul01(self, rhs)
    def :/(rhs: N1): Div01 = Div01(self, rhs)

    def :+(rhs: N2): Add02 = Add02(self, rhs)
    def :-(rhs: N2): Sub02 = Sub02(self, rhs)
    def :*(rhs: N2): Mul02 = Mul02(self, rhs)
    def :/(rhs: N2): Div02 = Div02(self, rhs)

    def unary_+(): Pos0 = Pos0(self)
    def unary_-(): Neg0 = Neg0(self)

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

    def +(rhs: N1): Add11 = { check(rhs, Add11.toString); Add11(self, rhs) }
    def -(rhs: N1): Sub11 = { check(rhs, Sub11.toString); Sub11(self, rhs) }
    def *(rhs: N1): Mul11 = { check(rhs, Mul11.toString); Mul11(self, rhs) }
    def /(rhs: N1): Div11 = { check(rhs, Div11.toString); Div11(self, rhs) }
    def dot(rhs: N1): Dot11 = { check(rhs, Dot11.toString); Dot11(self, rhs) }

    def :+(rhs: N0): Add10 = Add10(self, rhs)
    def :-(rhs: N0): Sub10 = Sub10(self, rhs)
    def :*(rhs: N0): Mul10 = Mul10(self, rhs)
    def :/(rhs: N0): Div10 = Div10(self, rhs)

    def :+(rhs: N2)(implicit d: DummyImplicit): Add12 = { check(rhs, Add12.toString); Add12(self, rhs) }
    def :-(rhs: N2)(implicit d: DummyImplicit): Sub12 = { check(rhs, Sub12.toString); Sub12(self, rhs) }
    def :*(rhs: N2)(implicit d: DummyImplicit): Mul12 = { check(rhs, Mul12.toString); Mul12(self, rhs) }
    def :/(rhs: N2)(implicit d: DummyImplicit): Div12 = { check(rhs, Div12.toString); Div12(self, rhs) }

    def unary_+(): Pos1 = Pos1(self)
    def unary_-(): Neg1 = Neg1(self)
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

    def +(rhs: N2): Add22 = { check(rhs, Add22.toString); Add22(self, rhs) }
    def -(rhs: N2): Sub22 = { check(rhs, Sub22.toString); Sub22(self, rhs) }
    def *(rhs: N2): Mul22 = { check(rhs, Mul22.toString); Mul22(self, rhs) }
    def /(rhs: N2): Div22 = { check(rhs, Div22.toString); Div22(self, rhs) }

    def :+(rhs: N0): Add20 = Add20(self, rhs)
    def :-(rhs: N0): Sub20 = Sub20(self, rhs)
    def :*(rhs: N0): Mul20 = Mul20(self, rhs)
    def :/(rhs: N0): Div20 = Div20(self, rhs)

    def :+(rhs: N1)(implicit d: DummyImplicit): Add21 = { check(rhs, Add21.toString); Add21(self, rhs) }
    def :-(rhs: N1)(implicit d: DummyImplicit): Sub21 = { check(rhs, Sub21.toString); Sub21(self, rhs) }
    def :*(rhs: N1)(implicit d: DummyImplicit): Mul21 = { check(rhs, Mul21.toString); Mul21(self, rhs) }
    def :/(rhs: N1)(implicit d: DummyImplicit): Div21 = { check(rhs, Div21.toString); Div21(self, rhs) }

    def matmul(rhs: N2): MatMul22 = { matmulCheck(rhs, MatMul22.toString()); MatMul22(self, rhs) }

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

class ShapeCheckException(a: Node[_], b: Node[_], op: String)
  extends Exception(
    s"$op cannot applicable for variables with shape pair ${a.shape} and ${b.shape}"
  )
