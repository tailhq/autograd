package com.kogecoo.scalaad.graph.bool

import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.graph.{BE0, BE1, BE2, Expr}
import com.kogecoo.scalaad.op.bool.{And, AndLeft, AndRight, Not, Or, OrLeft, OrRight}
import shapeless.Nat


trait BooleanExpr[N <: Nat] extends Expr[N] {

  def eval[R](implicit E: Eval[BooleanExpr[N], R]): R = E.eval(this)

}


object BooleanExpr {

  implicit class BooleanExpr0(val self: BE0) extends AnyVal {

    def &(rhs: BE0): BE0 = Apply2B(self, rhs, And)
    def |(rhs: BE0): BE0 = Apply2B(self, rhs, Or)

    def :&(rhs: BE1): BE1 = ElementwiseRightB(self, rhs, AndRight)
    def :|(rhs: BE1): BE1 = ElementwiseRightB(self, rhs, OrRight)

    def :&(rhs: BE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightB(self, rhs, AndRight)
    def :|(rhs: BE2)(implicit d: DummyImplicit): BE2 = ElementwiseRightB(self, rhs, OrRight)

    def unary_!(): BE0 = Apply1B(self, Not)

  }

  implicit class BooleanExpr1Op(val self: BE1) extends AnyVal {

    def &(rhs: BE1): BE1 = Apply2B(self, rhs, And)
    def |(rhs: BE1): BE1 = Apply2B(self, rhs, Or)

    def :&(rhs: BE0): BE1 = ElementwiseLeftB(self, rhs, AndLeft)
    def :|(rhs: BE0): BE1 = ElementwiseLeftB(self, rhs, OrLeft)

    def unary_!(): BE1 = Apply1B(self, Not)

  }

  implicit class BoolNode2Op(val self: BE2) extends AnyVal {

    def &(rhs: BE2): BE2 = Apply2B(self, rhs, And)
    def |(rhs: BE2): BE2 = Apply2B(self, rhs, Or)

    def :&(rhs: BE0): BE2 = ElementwiseLeftB(self, rhs, AndLeft)
    def :|(rhs: BE0): BE2 = ElementwiseLeftB(self, rhs, OrLeft)

    def unary_!(): BE2 = Apply1B(self, Not)

  }

}

