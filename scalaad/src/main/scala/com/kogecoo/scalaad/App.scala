package com.kogecoo.scalaad

import com.kogecoo.scalaad.graph.{One0, N0}
import com.kogecoo.scalaad.impl.std.Implicits._
import Predef.{any2stringadd => _, _}


object App {

  def main(args: Array[String]): Unit = {
    val x = Var(1.0)
    val y = x + Var(5.0)
    val z = y.eval

    println(z)
    println(y.forward[N0, N0](x))
    println(y.reverse[N0](One0()))
    //val g = One0[Double]()
    //Reverse.reverse(y, g)
  }
}
