package com.kogecoo.scalaad.algorithm

trait Value[T, V] {

  def value(t: T): V

}
