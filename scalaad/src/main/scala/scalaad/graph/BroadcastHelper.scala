package scalaad.graph

import scalaad.{Constraint, Shape}


// Use numpy's broadcast rule : http://docs.scipy.org/doc/numpy/reference/ufuncs.html#broadcasting
object BroadcastHelper {

  @throws[Exception]
  def computeOutputShape(shapes: Seq[Shape]): Shape = {
    val normalized = normalizeOrders(shapes)
    val outputShape = new Shape(maxLengthAxes(normalized))

    Constraint.satisfy(ensureBroadcastable(outputShape, normalized), s"Non broadcastable shapes : $shapes")
    outputShape
  }

  // rule 1
  private[this] def normalizeOrders(shapes: Seq[Shape]): Seq[Shape] = {
    val maxOrder = shapes.map(_.order).max
    shapes.map { s =>
      if (s.order < maxOrder) {
        s.prependAxes(maxOrder - s.order)
      } else s
    }
  }

  // rule 2
  // every elements of `shapes` required to be have same orders.
  private[this] def maxLengthAxes(shapes: Seq[Shape]): Seq[Int] = {
    Seq.range(0, shapes.head.order).map { i => shapes.map(_.at(i)).max }
  }

  // rule 3
  private[this] def ensureBroadcastable(output: Shape, shapes: Seq[Shape]): Boolean = {
    (0 until output.order).forall { i =>
      val ov = output.at(i)
      shapes.forall { shape =>
        val v = shape.at(i)
        v == 1 || v == ov
      }
    }
  }
}
