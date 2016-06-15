package com.kogecoo.scalaad


trait Value[T, V] {

  def value(t: T): V

}
