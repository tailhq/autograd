package com.kogecoo.scalaad.graph

import com.kogecoo.scalaad.{Shape, Tensor}


trait VarBase extends Apply0 {

  def forward(wrt: V): V = Eye(forwardOutputShape(wrt))

  def reverse(adj: V): Grad = Grad(this, adj)

}

case class Var(data: Tensor) extends VarBase { def shape: Shape = data.shape }


// Future Work
// class Placeholder(name: String, override val shape: Shape) extends VarBase
