package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{Apply0, VE}


sealed trait Operator[SO <: Shape]

trait NullaryOp[SO <: Shape] extends Operator[SO] {

  def deriv[S <: Shape]: VE[S] = Apply0[S](Zero)

}

trait UnaryOp[S <: Shape] extends Operator[S] {

  def deriv[VS <: Shape](v: VE[VS]): VE[VS]

}

trait BinaryOp[S <: Shape] extends Operator[S] {

  def deriv[VS <: Shape](l: VE[VS], r: VE[VS]): (VE[VS], VE[VS])

}

trait AsymmetricUnaryOp[SO <: Shape, SI <: Shape] extends Operator[SO] {

  def deriv(v: VE[SI]): VE[SO]

}

trait AsymmetricLeftBinaryOp[SL <: Shape, SR <: Shape] extends Operator[SL] {

  def deriv(l: VE[SL], r: VE[SR]): (VE[SL], VE[SL])

}

trait AsymmetricRightBinaryOp[SL <: Shape, SR <: Shape] extends Operator[SR] {

  def deriv(l: VE[SL], r: VE[SR]): (VE[SR], VE[SR])

}

