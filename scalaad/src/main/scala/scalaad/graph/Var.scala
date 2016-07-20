package scalaad.graph

import scalaad.{Shape, Tensor}


trait VarBase extends Elementwise0[Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = Eye(forwardOutputShape(wrt))

  def reverse(adj: DExpr[Real]): Grad = Grad(this, adj)

}

case class Var(data: Tensor[Real]) extends VarBase { def shape: Shape = data.shape }


// Future Work
// class Placeholder(name: String, override val shape: Shape) extends VarBase
