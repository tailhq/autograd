package scalaad.graph

import scalaad.{Eval, Shape}


trait Expr[D <: DType] {

  def shape: Shape

  def eval[R](implicit ev: Eval[Expr[D], R]): R = ev.eval(this)

}


object Expr {


  implicit class RichRealExpr(val self: DExpr[Real]) extends AnyVal {

    def unary_+(): DExpr[Real] = Pos(self)

    def unary_-(): DExpr[Real] = Neg(self)

    def :+(rhs: DExpr[Real]): DExpr[Real] = Add(self, rhs)

    def :-(rhs: DExpr[Real]): DExpr[Real] = Sub(self, rhs)

    def :*(rhs: DExpr[Real]): DExpr[Real] = Mul(self, rhs)

    def :/(rhs: DExpr[Real]): DExpr[Real] = Div(self, rhs)

    def dot(rhs: DExpr[Real]): DExpr[Real] = Dot(self, rhs)

    def :==(rhs: DExpr[Real]): Expr[Bool] = Eq(self, rhs)

    def :!=(rhs: DExpr[Real]): Expr[Bool] = Neq(self, rhs)

    def :<(rhs: DExpr[Real]): Expr[Bool] = Lt(self, rhs)

    def :<=(rhs: DExpr[Real]): Expr[Bool] = Lte(self, rhs)

    def :>(rhs: DExpr[Real]): Expr[Bool] = Gt(self, rhs)

    def :>=(rhs: DExpr[Real]): Expr[Bool] = Gte(self, rhs)
  }

  implicit class RichBoolExpr(val self: Expr[Bool]) extends AnyVal {

    // FIXME: check underlying type
    def :&(rhs: Expr[Bool]): Expr[Bool] = And(self, rhs)

    def :|(rhs: Expr[Bool]): Expr[Bool] = Or(self, rhs)

    def unary_!(): Expr[Bool] = Not(self)
  }

}


