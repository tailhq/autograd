package scalaad.graph

import scalaad.{Shape, Tensor}


trait VarBase extends Elementwise0 with Differentiable {

  def forward(wrt: DExpr): DExpr = Eye(forwardOutputShape(wrt))

  def reverse(adj: DExpr): Grad = Grad(this, adj)

}

case class Var(data: Tensor) extends VarBase { def shape: Shape = data.shape }


// Future Work
// class Placeholder(name: String, override val shape: Shape) extends VarBase
