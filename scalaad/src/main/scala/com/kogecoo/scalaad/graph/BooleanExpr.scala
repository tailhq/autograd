package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.op.{And, Not, Or}


trait BooleanExpr[S <: Shape] extends Expr[S] {

  def eval[V](implicit E: Eval[BooleanExpr[S], V]): V = E.eval(this)

}


object BooleanExpr {

  implicit class BooleanExpr0(val self: B0) extends AnyVal {

    def &(rhs: B0): B0 = Apply00B(self, rhs, And)
    def |(rhs: B0): B0 = Apply00B(self, rhs, Or)

    def :&(rhs: B1): B1 = Elementwise01B(self, rhs, And)
    def :|(rhs: B1): B1 = Elementwise01B(self, rhs, Or)

    def :&(rhs: B2)(implicit d: DummyImplicit): B2 = Elementwise02B(self, rhs, And)
    def :|(rhs: B2)(implicit d: DummyImplicit): B2 = Elementwise02B(self, rhs, Or)

    def unary_!(): B0 = Apply0B(self, Not)

  }

  implicit class BooleanExpr1Op(val self: B1) extends AnyVal {

    def &(rhs: B1): B1 = Elementwise11B(self, rhs, And)
    def |(rhs: B1): B1 = Elementwise11B(self, rhs, Or)

    def :&(rhs: B0): B1 = Elementwise10B(self, rhs, And)
    def :|(rhs: B0): B1 = Elementwise10B(self, rhs, Or)

    def unary_!(): B1 = Elementwise1B(self, Not)

  }

  implicit class BoolNode2Op(val self: B2) extends AnyVal {

    def &(rhs: B2): B2 = Elementwise22B(self, rhs, And)
    def |(rhs: B2): B2 = Elementwise22B(self, rhs, Or)

    def :&(rhs: B0): B2 = Elementwise20B(self, rhs, And)
    def :|(rhs: B0): B2 = Elementwise20B(self, rhs, Or)

    def unary_!(): B2 = Elementwise2B(self, Not)

  }

}

