package com.kogecoo.scalaad


trait Eval[N, V] {

  def eval(n: N): V

}

