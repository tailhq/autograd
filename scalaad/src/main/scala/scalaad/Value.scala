package scalaad

import scalaad.graph.DType


trait Value[D <: DType, R] {

  def value(t: Tensor[D]): R

}
