package com.kogecoo.scalaad

import shapeless.Nat.{_0, _1, _2}
import shapeless.{Nat, Sized}


/**
  * containers used for carrying
  * tensor shape (dimensions for each axis) parameter(s).
  */
class Shape[N <: Nat](shape: Sized[List[Int], N]) {

  override def toString: String = {
    val o = order
    val s = underlying.mkString(", ")
    s"Shape(order=$o, shape=($s))"
  }

  def at(i: Int): Int = shape.unsized(i)

  def underlying: List[Int] = shape.unsized

  def extend[M <: Nat, O <: Nat](other: Shape[M]): Shape[O] = {
    val extend = underlying ++ other.underlying
    new Shape[O](Sized.wrap[List[Int], O](extend))
  }

  def shrink[O <: Nat](axes: List[Int]): Shape[O] = {
    val shrunk = for (
      (s, i) <- underlying.zipWithIndex
      if !axes.contains(i)
    ) yield s
    new Shape[O](Sized.wrap[List[Int], O](shrunk))
  }

  def order: Int = shape.unsized.length

  def hasSamePrefixWith[M <: Nat](s: Shape[M]): Boolean = {
    underlying.take(s.order).zip(s.underlying).forall { case (i, j) => i == j }
  }

  def ==[M <: Nat](other: Shape[M]): Boolean = {
    order == other.order && underlying.zip(other.underlying).forall { case (i, j) => i == j }
  }

  def !=[M <: Nat](other: Shape[M]): Boolean = !(this == other)
}


object Shape {

  def apply[N <: Nat](underlying: List[Int]): Shape[N] = {
    new Shape[N](Sized.wrap[List[Int], N](underlying))
  }

}


object Shape0 {

  def apply(): Shape[_0] = new Shape[_0](Sized[List]())

}


object Shape1 {

  def apply(l: Int): Shape[_1] = new Shape[_1](Sized[List](l))

}


object Shape2 {

  def apply(r: Int, c: Int): Shape[_2] = Shape[_2](Sized[List](r, c))

}

