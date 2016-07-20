package scalaad.graph


/**
  * Unary Operators
  */

case class Pos(override val v: DExpr[Real]) extends Elementwise1[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = +v.forward(wrt)

  def reverse(adj: DExpr[Real]): Grad = v.reverse(+adj)

}


case class Neg(override val v: DExpr[Real]) extends Elementwise1[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = -v.forward(wrt)

  def reverse(adj: DExpr[Real]): Grad = v.reverse(-adj)

}


case class Identity(override val v: DExpr[Real]) extends Elementwise1[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = v.forward(wrt)

  def reverse(adj: DExpr[Real]): Grad = v.reverse(adj)

}


case class Sign(override val v: DExpr[Real]) extends Elementwise1[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = Sign(v.forward(wrt))

  def reverse(adj: DExpr[Real]): Grad = v.reverse(Sign(adj))

}

/**
  * Binary Operators
  */

case class Add(override val l: DExpr[Real], override val r: DExpr[Real]) extends Elementwise2[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = l.forward(wrt) :+ r.forward(wrt)

  def reverse(adj: DExpr[Real]): Grad = l.reverse(adj) ++ r.reverse(adj)

}


case class Sub(override val l: DExpr[Real], override val r: DExpr[Real]) extends Elementwise2[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = l.forward(wrt) :+ -r.forward(wrt)

  def reverse(adj: DExpr[Real]): Grad = l.reverse(adj) ++ r.reverse(-adj)

}


case class Mul(override val l: DExpr[Real], override val r: DExpr[Real]) extends Elementwise2[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = (l.forward(wrt) :* r) :+ (l :* r.forward(wrt))

  def reverse(adj: DExpr[Real]): Grad = l.reverse(adj :* r) ++ r.reverse(l :* adj)

}


case class Div(override val l: DExpr[Real], override val r: DExpr[Real]) extends Elementwise2[Real, Real] with Differentiable[Real] {

  def forward(wrt: DExpr[Real]): DExpr[Real] = {
    val a = l.forward(wrt) :/ r
    val b = (-l :* r.forward(wrt)) :/ (r :* r)
    a :+ b
  }

  def reverse(adj: DExpr[Real]): Grad = {
    val a = adj :/ r
    val b = (-l :* adj) :/ (r :* r)
    l.reverse(a) ++ r.reverse(b)
  }

}

