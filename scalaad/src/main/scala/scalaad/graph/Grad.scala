package scalaad.graph

import scala.Predef.{any2stringadd => _, _}


class Grad(val m: Map[DExpr[Real], DExpr[Real]]) {

  def size: Int = m.size

  def apply(x: DExpr[Real]): Option[DExpr[Real]] = m.get(x)

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
    }).toMap[DExpr[Real], DExpr[Real]]
    new Grad(m)
  }

}

object Grad {

  def apply(p: DExpr[Real], v: DExpr[Real]): Grad = {
    val m = Map[DExpr[Real], DExpr[Real]](p -> v)
    new Grad(m)
  }

  def empty: Grad = {
    val m = Map.empty[DExpr[Real], DExpr[Real]]
    new Grad(m)
  }

}

