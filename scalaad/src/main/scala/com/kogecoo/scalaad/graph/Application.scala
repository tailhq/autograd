package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.analyze.{Analyzing, ElementwiseLeftEqn2, ElementwiseRightEqn2, Eqn0, Eqn1, Eqn2, FillEqn1, FoldEqn1, FoldEqn2, Param, WhereEqn}
import com.kogecoo.scalaad.graph.bool.BooleanExpr
import com.kogecoo.scalaad.op.{BinaryOp, NullaryOp, Op0, Op00, UnaryOp, Zero}
import com.kogecoo.scalaad.{S0, Shape, Shape0}


/**
  * represents applying binary operation, which takes 2 Exprs arguments.
  *
  * @tparam S a shape of this Expr
  * @tparam L a shape of left Expr
  * @tparam R a shape of right Expr
  */
trait Application2[S <: Shape, L <: Shape, R <: Shape] extends ValueExpr[S] {
  def shape: S
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left and output Expr are the same type of shape.
  *
  * @tparam L a shape of left and output Expr
  * @tparam R a shape of right Expr
  */
trait LeftShapedApplication2[L <: Shape, R <: Shape] extends Application2[L, L, R] {
  def shape: L = l.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its right and output Expr are the same type of shape.
  *
  * @tparam L a shape of left Expr
  * @tparam R a shape of right and output Expr
  */
trait RightShapedApplication2[L <: Shape, R <: Shape] extends Application2[R, L, R] {
  def shape: R = r.shape
  def l: ValueExpr[L]
  def r: ValueExpr[R]
}

/**
  * specialized Application2, its left, right and output Expr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedApplication2[S <: Shape] extends Application2[S, S, S] {
  def shape: S = l.shape
  def l: ValueExpr[S]
  def r: ValueExpr[S]
}

/**
  * represents applying unary operation, which takes 1 Expr as argument.
  *
  * @tparam O type of shape for output Expr
  * @tparam S type of shape for argument Expr
  */
trait Application1[O <: Shape, S <: Shape] extends ValueExpr[O] {
  def shape: O
  def v: ValueExpr[S]

}

/**
  * specialized Application1, its input and output Expr are the same type of shape.
  *
  * @tparam S type of shape for left, right and output Expr
  */
trait CommonShapedApplication1[S <: Shape] extends Application1[S, S] {
  def shape: S = v.shape
  def v: ValueExpr[S]
}

// Nullary Application

case class Apply0[S <: Shape](op: NullaryOp[S0]) extends CommonShapedApplication1[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(Eqn0[S](op))

  def forward[W, O] = Apply0(Zero)

}


// Unary Application

case class Apply1[S <: Shape](v: VE[S], op: Op0) extends CommonShapedApplication1[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(Eqn1(v.analyze(a), op))

  def forward[W, O] = v.forward * op.deriv(v)

}


case class Fold1[SI1 <: Shape](v: VE[SI1], op: UnaryOp[S0, SI1]) extends Application1[S0, SI1] {

  def shape: S0 = Shape0()

  def analyze(a: Analyzing): Param[S0] = a.addEqn(FoldEqn1(v.analyze(a), op))

}


case class Fill[SO <: Shape](v: VE0, shape: SO) extends Application1[SO, S0] {

  def analyze(a: Analyzing): Param[SO] = a.addEqn(FillEqn1(v.analyze(a), shape))

}



// Binary Application

case class Apply2[S <: Shape](l: VE[S], r: VE[S], op: Op00) extends CommonShapedApplication2[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(Eqn2(l.analyze(a), r.analyze(a), op))

}


case class ElementwiseLeft[S <: Shape](l: VE[S], r: VE0, op: Op00) extends LeftShapedApplication2[S, S0] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(ElementwiseLeftEqn2(l.analyze(a), r.analyze(a), op))
}


case class ElementwiseRight[S <: Shape](l: VE0, r: VE[S], op: Op00) extends RightShapedApplication2[S0, S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(ElementwiseRightEqn2(l.analyze(a), r.analyze(a), op))
}


case class Fold2[SI1 <: Shape, SI2 <: Shape](l: VE[SI1], r: VE[SI2], op: BinaryOp[S0, SI1, SI2]) extends Application2[S0, SI1, SI2] {

  def shape: S0 = Shape0()

  def analyze(a: Analyzing): Param[S0] = a.addEqn(FoldEqn2(l.analyze(a), r.analyze(a), op))

}


case class Where[S <:  Shape](cond: BooleanExpr[S], l: VE[S], r: VE[S]) extends CommonShapedApplication2[S] {

  def analyze(a: Analyzing): Param[S] = a.addEqn(WhereEqn[S](cond, l.analyze(a), r.analyze(a)))
}
