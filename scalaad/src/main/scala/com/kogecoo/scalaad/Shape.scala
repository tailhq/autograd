package com.kogecoo.scalaad

import shapeless.Nat.{_0, _1, _2}
import shapeless.ops.nat.Sum
import shapeless.{Nat, Sized}

/**
  * containers used for carrying
  * tensor shape (dimensions for each axis) parameter(s).
  */
class Shape[N <: Nat](val underlying: Sized[Array[Int], N]) {

  def apply(i: Int): Int = underlying.unsized(i)

  def append[M <: Nat, O <: Nat](other: Shape[M])(implicit sum: Sum.Aux[N, M, O]): Shape[O] = {
    val concat = underlying.unsized ++ other.underlying.unsized
    new Shape[O](Sized.wrap[Array[Int], O](concat))
  }
}

object Shape0 {

  def apply(): Shape[_0] = new Shape[_0](Sized.wrap[Array[Int], _0](Array[Int]()))

}

// default shape is a column vector
object Shape1 {

  def apply(l: Int): Shape[_1] = new Shape[_1](Sized.wrap[Array[Int], _1](Array(l)))

}

object Shape2 {

  def apply(r: Int, c: Int): Shape[_2] = new Shape[_2](Sized.wrap[Array[Int], _2](Array(r, c)))

}

