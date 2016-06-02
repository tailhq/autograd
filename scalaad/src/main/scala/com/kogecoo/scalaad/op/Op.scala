package com.kogecoo.scalaad.op

import com.kogecoo.scalaad.Shape
import com.kogecoo.scalaad.graph.{Apply0, VE}


sealed trait Operator[SO <: Shape]

trait NullaryOp[SO <: Shape] extends Operator[SO] {

  def deriv[S <: Shape](v: VE[S]): VE[S] = Apply0(Zero)

}

trait UnaryOp[SO <: Shape, SI <: Shape] extends Operator[SO] {

  def deriv[VS <: Shape](v: VE[VS]): VE[VS]

}

trait BinaryOp[SO <: Shape, SI1 <: Shape, SI2 <: Shape] extends Operator[SO] {

  def deriv[S <: Shape](l: VE[S], r: VE[S]): (VE[S], VE[S])

}
