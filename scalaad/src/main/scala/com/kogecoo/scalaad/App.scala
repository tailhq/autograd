package com.kogecoo.scalaad

import com.kogecoo.scalaad.analyze.Analyzing
import com.kogecoo.scalaad.graph.{One0, VE0}
import com.kogecoo.scalaad.impl.std.Implicits._

import Predef.{any2stringadd => _, _}


object App {

  def main(args: Array[String]): Unit = {

    val x = Var(1.0)
    val y = x + Var(5.0)
    println(y)
    val z = y.eval

    val a = new Analyzing()
    println(y.analyze(a))
    val r = a.result()
    r.eqns.map { println }
    r.params.map { println }

    println(a.result())
    println(z)
    //println(y.forward[V0, V0](x))
    //println(y.reverse[V0](One0()))

    //val g = One0[Double]()
    //Reverse.reverse(y, g)
  }
}
