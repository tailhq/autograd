package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad._


trait ConstBase extends Apply0 {

  def forward(wrt: V): V = Zero(forwardOutputShape(wrt))

  def reverse(adj: V): Grad = Grad.empty

}


case class Zero(shape: Shape) extends ConstBase

case class Half(shape: Shape) extends ConstBase

case class One(shape: Shape) extends ConstBase

case class Two(shape: Shape) extends ConstBase


case class Const(data: Tensor) extends ConstBase { def shape: Shape = data.shape }


@throws[Exception]
case class Eye(shape: Shape) extends ConstBase {

  Constraint.allAxesHaveCommonLength(shape)

}


@throws[Exception]
case class Diag(diagVec: Tensor, order: Int) extends ConstBase {

  Constraint.satisfy(diagVec.shape.order == 1, s"The order of a reference vector for Diag tensor needs to be == 1 but $diagVec")

  Constraint.satisfy(shape.order > 1, s"The order of a shape of argument for Diag needs to be >= 2 but $shape")

  def shape: Shape = Shape(Seq.fill(order)(diagVec.shape.at(0)):_*)

}

