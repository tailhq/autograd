package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.op.bool.{AsymmetricLeftBinaryBooleanOp, AsymmetricRightBinaryBooleanOp}
import com.kogecoo.scalaad.op.{Add, AsymmetricLeftBinaryOp, AsymmetricRightBinaryOp, BinaryOp, MulLeft, MulRight, NullaryOp, UnaryOp, Zero}
import shapeless.Nat
import shapeless.ops.nat.LT.<
import shapeless.ops.nat.Sum


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam N a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[N <: Nat, L <: Nat, R <: Nat] extends ValueExpr[N] {
  def shape: Shape[N]
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left and output Expr are the same type of shape.
  *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[L <: Nat, R <: Nat] extends Application2[L, L, R] {
  def shape: Shape[L] = l.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[L <: Nat, R <: Nat] extends Application2[R, L, R] {
  def shape: Shape[R] = r.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedApplication2[N <: Nat] extends Application2[N, N, N] {
  def shape: Shape[N] = l.shape
  def l: ValueExpr[N]
  def r: ValueExpr[N]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam O type of shape for output Expr
  * @tparam N type of shape for argument Expr
  */
trait Application1[O <: Nat, N <: Nat] extends ValueExpr[O] {
  def shape: Shape[O]
  def v: ValueExpr[N]

}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  *
  * @tparam N type of shape for left, right and output Expr
  */
trait CommonShapedApplication1[N <: Nat] extends Application1[N, N] {
  def shape: Shape[N] = v.shape
  def v: ValueExpr[N]
}

// Nullary Application

case class Apply0[N <: Nat](shape: Shape[N], op: NullaryOp) extends ValueExpr[N] {

  def forward[W <: Nat, O <: Nat](wrt: VE[W])(implicit s: Sum.Aux[N, W, O]): VE[O] = {
    val newShape = shape.append(wrt.shape)
    Apply0(newShape, Zero)
  }

}


// Unary Application

case class Apply1[N <: Nat](v: VE[N], op: UnaryOp) extends CommonShapedApplication1[N] {

  def forward[W <: Nat, O <: Nat](wrt: VE[W])(implicit s: Sum.Aux[N, W, O]): VE[O] = {
    ElementwiseLeft(v.forward[W, O](wrt), op.deriv[N](v), MulLeft)
  }

}

/*
case class Fold1[SI1 <: Nat](v: VE[SI1], op: UnaryOp) extends Application1[S0, SI1] {

  def shape: Shape[S0] = Shape0()

}


case class Fill[SO <: Nat](v: VE0, shape: SO) extends Application1[SO, S0] {

}
*/


// Binary Application

case class Apply2[N <: Nat](l: VE[N], r: VE[N], op: BinaryOp) extends CommonShapedApplication2[N] {

  def forward[W <: Nat, O <: Nat](wrt: VE[W])(implicit sum: Sum.Aux[N, W, O]) = {
    val (dl: VE[N], dr: VE[N]) = op.deriv[N](l, r)
    val a = ElementwiseLeft(l.forward[W, O](wrt), dr, MulLeft)
    val b = ElementwiseRight(dl, r.forward[W, O](wrt), MulRight)
    Apply2(a, b, Add)
  }

}


case class ElementwiseLeft[L <: Nat, R <: Nat](l: VE[L], r: VE[R], op: AsymmetricLeftBinaryOp) extends LeftShapedApplication2[L, R] {

  def forward[W <: Nat, O <: Nat](wrt: VE[W])(implicit sum: Sum.Aux[L, W, O]) = {
    val (dl, dr) = op.deriv[L, R](l, r)
    val a = ElementwiseLeft(l.forward[W, O](wrt), dr, MulLeft)
    val b = ElementwiseRight(dl, r.forward[W, O](wrt), MulRight)
    Apply2(a, b, Add)
  }

}


case class ElementwiseRight[L <: Nat, R <: Nat](l: VE[L], r: VE[R], op: AsymmetricRightBinaryOp) extends RightShapedApplication2[L, R] {

  def forward[W <: Nat, O <: Nat](wrt: VE[W])(implicit sum: Sum.Aux[R, W, O]) = {
    val (dl, dr) = op.deriv[L, R](l, r)
    val a = ElementwiseLeft(l.forward[W, O](wrt), dr, MulLeft)
    val b = ElementwiseRight(dl, r.forward[W, O](wrt), MulRight)
    Apply2(a, b, Add)
  }

}


/*
case class Fold2[SI1 <: Nat, SI2 <: Nat](l: VE[SI1], r: VE[SI2], op: BinaryOp[S0, SI1, SI2]) extends Application2[S0, SI1, SI2] {

  def shape: Shape[S0] = Shape0()

}


case class Where[N <:  Nat](cond: BooleanExpr[N], l: VE[N], r: VE[N]) extends CommonShapedApplication2[N] {
}
*/
