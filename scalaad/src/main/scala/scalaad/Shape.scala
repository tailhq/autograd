package scalaad

/**
  * containers used for carrying
  * tensor shape (dimensions for each axis) parameter(s).
  */
class Shape(val underlying: Seq[Int]) {

  Constraint.satisfy(underlying.forall(_ >= 0), s"Lengths for every axis of Shape needs to be >= 0 : $underlying")

  def ==(other: Shape): Boolean = {
    order == other.order && underlying.zip(other.underlying).forall { case (i, j) => i == j }
  }

  def !=(other: Shape): Boolean = !(this == other)

  def at(axis: Int): Int = underlying(normalizeNegativeAxis(axis))

  def slice(from: Int, until: Int): Shape = {
    new Shape(underlying.slice(normalizeNegativeAxis(from), normalizeNegativeAxis(until)))
  }

  def order: Int = underlying.length

  def concat(other: Shape): Shape = new Shape(underlying ++ other.underlying)

  def prependAxes(numAxes: Int): Shape = {
    Constraint.satisfy(numAxes >= 0, s"numAxes $numAxes needs to be >= 0")
    new Shape(Seq.fill(numAxes)(1) ++ underlying)
  }

  @throws[Exception]
  def removeAxes(axes: Seq[Int]): Shape = {
    Constraint.indicatesValidAxes(this, axes)

    val shrunk = for (
      (s, i) <- underlying.zipWithIndex
      if !axes.contains(i)
    ) yield s
    new Shape(shrunk)
  }

  override def toString: String = {
    val o = order
    val s = underlying.mkString(", ")
    s"Shape(order=$o, shape=($s))"
  }

  private[this] def normalizeNegativeAxis(axis: Int): Int = {
    if (axis >= 0) {
      axis
    } else {
      order + axis
    }
  }

}


object Shape {

  def apply(underlying: Int*): Shape = new Shape(underlying)

}
