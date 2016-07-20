package scalaad


trait Eval[N, V] {

  def eval(n: N): V

}
