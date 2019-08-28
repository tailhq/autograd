# AutoGrad

Automatic differentiation for Scala

## Use

```scala
import io.github.tailabs.autograd.graph.Var  // always need to import
import io.github.tailabs.autograd.ScalarRule.Implicits._  // when x is a scalar variable

val x = Var(5.0)
val y = Var(3.0)
val z = 1 * x * sin(x) * 2 + y * x * 3

println(z)
println(z.deriv(x))  // forward-mode automatic differentiation
println(z.deriv(y))

println(z.grad())    // reverse-mode automatic differentiation 
println(x.gradient)  // we can get partial differentiation through gradient after run grad()
println(y.gradient)
```
