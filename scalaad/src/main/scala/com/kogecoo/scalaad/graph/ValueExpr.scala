package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Eval
import com.kogecoo.scalaad.graph.bool.{Eq, Gt, Gte, Lt, Lte, Neq}



trait ValueExpr extends Expr {

  def forward(wrt: ValueExpr): ValueExpr

  def reverse(adj: ValueExpr): Grad

  def eval[R](implicit E: Eval[ValueExpr, R]): R = E.eval(this)

}


object ValueExpr {


  implicit class RichValueExpr(val self: V) extends AnyVal {

    def unary_+(): V = Pos(self)
    def unary_-(): V = Neg(self)

    def :+(rhs: V): V = Add(self, rhs)
    def :-(rhs: V): V = Sub(self, rhs)
    def :*(rhs: V): V = Mul(self, rhs)
    def :/(rhs: V): V = Div(self, rhs)

    def dot(rhs: V): V = Dot(self, rhs)

    def :==(rhs: V): B = Eq (self, rhs)
    def :!=(rhs: V): B = Neq(self, rhs)
    def :< (rhs: V): B = Lt (self, rhs)
    def :<=(rhs: V): B = Lte(self, rhs)
    def :> (rhs: V): B = Gt (self, rhs)
    def :>=(rhs: V): B = Gte(self, rhs)

  }

}

