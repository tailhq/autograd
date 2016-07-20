package scalaad.example


object Application {

  def main(args: Array[String]) = {
    scalarExample()
    breezeExample()
    nd4jExample()
  }

  def scalarExample() = {
    import scalaad.Shorthands.math._
    import scalaad.impl.std.Implicits._
    import scalaad.impl.std._

    val x = Var(5.0)
    val y = Var(3.0)
    val z = x :* sin(x) :* 2 :+ y :* x :* 3

    println(z)
    println(z.forward(x).eval[T0])  // forward-mode automatic differentiation
    println(z.forward(y).eval[T0])

    val grads = z.reverse(Const(1)) // reverse-mode automatic differentiation
    println(grads)
    println(s"$x -> ${grads(x)}")  // we can get partial differentiation through gradient after run grad()
    println(s"$y -> ${grads(y)}")
  }

  def breezeExample() = {

    import breeze.linalg.DenseVector

    import scalaad.Shorthands.math._
    import scalaad.impl.breeze.Implicits._
    import scalaad.impl.breeze._

    val x = Var(DenseVector(1.0, 2.0, 3.0))

    val y = sin(x) :* 2 :+ x :* 3

    println(y)
    println(y.forward(x).eval[T2])

    val grads = y.reverse(Const(DenseVector(1.0, 1.0, 1.0)))
    println(s"$x -> ${grads(x)}")

  }

  def nd4jExample() = {
    import org.nd4s.Implicits._

    import scalaad.Shorthands.math._
    import scalaad.impl.nd4j.Implicits._
    import scalaad.impl.nd4j._

    val x = Var((1 to 9).asNDArray(1, 3))

    val y = sin(x) :* 2 :+ x :* 3

    println(y)
    println(y.forward(x).eval[T])

    val grads = y.reverse(Const(1))
    println(s"$x -> ${grads(x)}")
  }

}
