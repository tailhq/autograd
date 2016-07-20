package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Eval, Shape}


trait Expr {

  def shape: Shape

  def eval[R](implicit ev: Eval[Expr, R]): R = ev.eval(this)

}


object Expr {


  implicit class RichExpr(val self: DExpr) extends AnyVal {

    def unary_+(): DExpr = Pos(self)
    def unary_-(): DExpr = Neg(self)

    def :+(rhs: DExpr): DExpr = Add(self, rhs)
    def :-(rhs: DExpr): DExpr = Sub(self, rhs)
    def :*(rhs: DExpr): DExpr = Mul(self, rhs)
    def :/(rhs: DExpr): DExpr = Div(self, rhs)

    def dot(rhs: DExpr): DExpr = Dot(self, rhs)

    def :==(rhs: DExpr): Expr = Eq (self, rhs)
    def :!=(rhs: DExpr): Expr = Neq(self, rhs)
    def :< (rhs: DExpr): Expr = Lt (self, rhs)
    def :<=(rhs: DExpr): Expr = Lte(self, rhs)
    def :> (rhs: DExpr): Expr = Gt (self, rhs)
    def :>=(rhs: DExpr): Expr = Gte(self, rhs)


    // FIXME: check underlying type
    def :&(rhs: Expr): Expr = And(self, rhs)
    def :|(rhs: Expr): Expr = Or(self, rhs)

    def unary_!(): Expr = Not(self)

  }

}


