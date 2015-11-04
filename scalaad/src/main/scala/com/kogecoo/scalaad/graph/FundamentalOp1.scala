package com.kogecoo.scalaad.graph


case class Pos(v: V) extends Elementwise1(v) {

  def forward(wrt: V): V = +v.forward(wrt)

  def reverse(adj: V): Grad = v.reverse(+adj)

}


case class Neg(v: V) extends Elementwise1(v) {

  def forward(wrt: V): V = -v.forward(wrt)

  def reverse(adj: V): Grad = v.reverse(-adj)

}


case class Identity(v: V) extends Elementwise1(v) {

  def forward(wrt: V): V = v.forward(wrt)

  def reverse(adj: V): Grad = v.reverse(adj)

}


case class Sign(v: V) extends Elementwise1(v) {

  def forward(wrt: V): V = Sign(v.forward(wrt))

  def reverse(adj: V): Grad = v.reverse(Sign(adj))

}
