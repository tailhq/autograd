package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.{Mul, UnaryFoldOp, UnaryOp}
import shapeless.Nat
import shapeless.Witness.Lt
import shapeless.ops.nat.LT.<
import shapeless.ops.nat.LTEq.<=

object UnsafeDSL {

  object Implicits {

    implicit class RichValueExpr[N <: Nat](val self: V[N]) extends AnyVal {

      /*
      def :+[M <: Nat](rhs: V[M]): V[N] = ElementwiseLeft(self, rhs, Add)
      def :-[M <: Nat](rhs: V[M]): V[N] = ElementwiseLeft(self, rhs, Sub)
      def :*[M <: Nat](rhs: V[M]): V[N] = ElementwiseLeft(self, rhs, Mul)
      def :/[M <: Nat](rhs: V[M]): V[N] = ElementwiseLeft(self, rhs, Div)
      */

    }

  }
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam N type of shape for output Expr
  * @tparam I type of shape for argument Expr
  */
trait Application1[N <: Nat, I <: Nat] extends ValueExpr[N] {

  def shape: Shape[N]

  def v: ValueExpr[I]

}


// Unary Application

case class Apply1[N <: Nat](v: V[N], op: UnaryOp) extends Application1[N, N] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = {
    ElementwiseLeft(v._forward[W, O](wrt), op.deriv[N](v), Mul)
  }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G])(implicit ev: N <= G): Unit = {
    v._reverse[G](g :* op.deriv[N](v), builder)
  }

}

/*
case class Fold1[N <: Nat, I <: Nat](v: V[I], override val shape: Shape[N], op: UnaryFoldOp) extends Application1[N, I] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}


case class Expand1[N <: Nat, I <: Nat](v: V[I], shape: Shape[N]) extends Application1[N, I] {

  def _forward[W <: Nat, O <: Nat](wrt: V[W]): V[O] = { }

  def _reverse[G <: Nat](g: ValueExpr[G], builder: GradBuilder[G]): Unit = { }

}

*/
