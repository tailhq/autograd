package com.kogecoo.scalaad.graph

import scala.language.higherKinds


trait UnaryOp[U[_], T] extends Node[U, T] {
  val v: Node[U, T]
}

trait BinaryOp[U[_], T] extends Node[U, T] {
  val lhs: Node[U, T]
  val rhs: Node[U, T]
}
