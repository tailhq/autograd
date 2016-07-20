package scalaad

import scalaad.graph.DType


trait Tensor[D <: DType] {

  def shape: Shape

  def value[R](implicit v: Value[D, R]): R = v.value(this)

}
