package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.N1

/**
  * containers used for carrying
  * tensor shape (dimensions for each axis) parameter(s).
  */
trait Shape

case class Shape0() extends Shape

// default shape is a column vector
case class Shape1(_1: Int) extends Shape

case class Shape2(_1: Int, _2: Int) extends Shape


/**
  * shorthands for making Shape2 from two Shape1
  */
object Shape2 {

  def apply(row: Shape1, col: Shape1): Shape2 = Shape2(row._1, col._1)
  def apply(row: N1, col: N1): Shape2 = Shape2(row.shape._1, col.shape._1)

}


// Future work: case class Shape[K <: Nat](s: Sized[Seq[Int], K]) extends Shape
