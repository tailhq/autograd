package com.kogecoo.scalaad.impl.std

import com.kogecoo.scalaad.Tensor0


case class StdScalar(data: Double) extends Tensor0 {

  def toStdFloat: Float = data.toFloat

  def toStdDouble: Double = data

}
