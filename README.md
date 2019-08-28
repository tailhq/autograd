# AutoGrad

Automatic differentiation for Scala

## Use

```scala
import io.github.tailabs.autograd.graph.Var  // always need to import
import io.github.tailabs.autograd.ScalarRule.Implicits._  // when x is a scalar variable

val x = Var(5.0)
val y = 2 * x + 3 * x * y

// forward-mode automatic differentiation
// partial differentiation w.r.t x
println(y.deriv(x))

// reverse-mode automatic differentiation computes a gradient
println(y.grad())

// we can get partial differentiation through `gradient` after running grad()
println(x.gradient)
println(y.gradient)
```
