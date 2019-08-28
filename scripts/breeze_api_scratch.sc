import $ivy.`io.github.transcendent-ai-labs::autograd:0.0.1-SNAPSHOT`
import io.github.tailabs.autograd.graph._ 
import io.github.tailabs.autograd.breeze.BreezeRule.Implicits._ 

val x = Var(DenseVector(1.0, 2.0, 3.0)) 

val y = sin(x) * 2 + x * 3


println("Y is: ")
pprint.pprintln(y)

println("Gradient of Y is: ")
pprint.pprintln(y.grad())