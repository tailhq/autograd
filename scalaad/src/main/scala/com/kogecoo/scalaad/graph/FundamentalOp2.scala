package com.kogecoo.scalaad.graph


case class Add(l: V, r: V) extends Elementwise2(l, r) {

  def forward(wrt: V): V = l.forward(wrt) :+ r.forward(wrt)

  def reverse(adj: V): Grad = l.reverse(adj) ++ r.reverse(adj)

}


case class Sub(l: V, r: V) extends Elementwise2(l, r) {

  def forward(wrt: V): V = l.forward(wrt) :+ -r.forward(wrt)

  def reverse(adj: V): Grad = l.reverse(adj) ++ r.reverse(-adj)

}


case class Mul(l: V, r: V) extends Elementwise2(l, r) {

  def forward(wrt: V): V = (l.forward(wrt) :* r) :+ (l :* r.forward(wrt))

  def reverse(adj: V): Grad = l.reverse(adj :* r) ++ r.reverse(l :* adj)

}


case class Div(l: V, r: V) extends Elementwise2(l, r) {

  def forward(wrt: V): V = {
    val a = l.forward(wrt) :/ r
    val b = (-l :* r.forward(wrt)) :/ (r :* r)
    a :+ b
  }

  def reverse(adj: V): Grad = {
    val a = adj :/ r
    val b = (-l :* adj) :/ (r :* r)
    l.reverse(a) ++ r.reverse(b)
  }

}
