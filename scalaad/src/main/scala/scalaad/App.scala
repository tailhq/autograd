package scalaad

import scalaad.impl.std.Implicits._
import scalaad.impl.std.T0


object App {

  def main(args: Array[String]): Unit = {

    val x = Var(1.0)
    val y = x :+ Var(5.0)
    println(y)
    val z = y.eval[T0]

    println(z)
    //println(y.forward[V0, V0](x))
    //println(y.reverse[V0](One0()))

    //val g = One0[Double]()
    //Reverse.reverse(y, g)
  }
}
