package com.kogecoo.scalaad.graph

import scala.Predef.{any2stringadd => _, _}


class Grad(val m: Map[V, V]) {

  def size: Int = m.size

  def apply(x: V): Option[V] = m.get(x)

  def ++(other: Grad): Grad = {
    val keys = this.m.keySet | other.m.keySet
    val m = (for {
      k <-  keys
      v1 =  this.m.get(k)
      v2 =  other.m.get(k)
      if v1.isDefined || v2.isDefined
    } yield {
      if (v1.isDefined && v2.isDefined) {
        (k, v1.get :+ v2.get)
      } else if (v1.isDefined) {
        (k, v1.get)
      } else {
        (k, v2.get)
      }
    }).toMap[V, V]
    new Grad(m)
  }

}

object Grad {

  def apply(p: V, v: V): Grad = {
    val m = Map[V, V](p -> v)
    new Grad(m)
  }

  def empty: Grad = {
    val m = Map.empty[V, V]
    new Grad(m)
  }

}

