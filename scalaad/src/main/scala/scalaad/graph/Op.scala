package scalaad.graph


/**
  * Unary Operators
  */

case class Pos(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = +v.forward(wrt)

  def reverse(adj: DExpr): Grad = v.reverse(+adj)

}


case class Neg(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = -v.forward(wrt)

  def reverse(adj: DExpr): Grad = v.reverse(-adj)

}


case class Identity(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = v.forward(wrt)

  def reverse(adj: DExpr): Grad = v.reverse(adj)

}


case class Sign(override val v: DExpr) extends Elementwise1 with Differentiable {

  def forward(wrt: DExpr): DExpr = Sign(v.forward(wrt))

  def reverse(adj: DExpr): Grad = v.reverse(Sign(adj))

}

/**
  * Binary Operators
  */

case class Add(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = l.forward(wrt) :+ r.forward(wrt)

  def reverse(adj: DExpr): Grad = l.reverse(adj) ++ r.reverse(adj)

}


case class Sub(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = l.forward(wrt) :+ -r.forward(wrt)

  def reverse(adj: DExpr): Grad = l.reverse(adj) ++ r.reverse(-adj)

}


case class Mul(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = (l.forward(wrt) :* r) :+ (l :* r.forward(wrt))

  def reverse(adj: DExpr): Grad = l.reverse(adj :* r) ++ r.reverse(l :* adj)

}


case class Div(override val l: DExpr, override val r: DExpr) extends Elementwise2 with Differentiable {

  def forward(wrt: DExpr): DExpr = {
    val a = l.forward(wrt) :/ r
    val b = (-l :* r.forward(wrt)) :/ (r :* r)
    a :+ b
  }

  def reverse(adj: DExpr): Grad = {
    val a = adj :/ r
    val b = (-l :* adj) :/ (r :* r)
    l.reverse(a) ++ r.reverse(b)
  }

}
