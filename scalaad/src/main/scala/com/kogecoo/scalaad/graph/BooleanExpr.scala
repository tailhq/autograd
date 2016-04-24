package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.algorithm.Eval
import com.kogecoo.scalaad.op.{And00, Not0, Or00}


trait BooleanExpr[S <: Shape] extends Expr[S] {

  val shape: S

  def eval[V](implicit E: Eval[BooleanExpr[S], V]): V = E.eval(this)

}


object BooleanExpr {

  implicit class BooleanExpr0(val self: B0) extends AnyVal {

    def &(rhs: B0): B0 = Apply00(self, rhs, And00)
    def |(rhs: B0): B0 = Apply00(self, rhs, Or00)

    def :&(rhs: B1): B1 = Broadcast01(self, rhs, And00)
    def :|(rhs: B1): B1 = Broadcast01(self, rhs, Or00)

    def :&(rhs: B2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, And00)
    def :|(rhs: B2)(implicit d: DummyImplicit): B2 = Broadcast02(self, rhs, Or00)

    def unary_!(): B0 = Apply0(self, Not0)

  }

  implicit class BooleanExpr1Op(val self: B1) extends AnyVal {

    def &(rhs: B1): B1 = Elementwise11(self, rhs, And00)
    def |(rhs: B1): B1 = Elementwise11(self, rhs, Or00)

    def :&(rhs: B0): B1 = Broadcast10(self, rhs, And10)
    def :|(rhs: B0): B1 = Broadcast10(self, rhs, Or10)

    //def :&(rhs: B2): B2 = And12(self, rhs)
    //def :|(rhs: B2): B2 = Or12 (self, rhs)

    def unary_!(): B1 = Broadcast1(self, Not0)

  }

  implicit class BoolNode2Op(val self: B2) extends AnyVal {

    def &(rhs: B2): B2 = Elementwise22(self, rhs, And00)
    def |(rhs: B2): B2 = Elementwise22(self, rhs, Or00)

    def :&(rhs: B0): B2 = Broadcast20(self, rhs, And00)
    def :|(rhs: B0): B2 = Broadcast20(self, rhs, Or00)

    //def :&(rhs: B1): B2 = And21(self, rhs)
    //def :|(rhs: B1): B2 = Or21 (self, rhs)

    def unary_!(): B2 = Broadcast2(self, Not0)

  }

}

