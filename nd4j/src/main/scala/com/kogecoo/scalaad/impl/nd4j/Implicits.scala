package com.kogecoo.scalaad.impl.nd4j

import com.kogecoo.scalaad.impl.std.{StdScalarEval, StdScalarValue}


object Implicits extends StdScalarEval
                    with StdScalarValue
                    with Nd4jVectorEval
                    with Nd4jVectorValue
                    with Nd4jMatrixEval
                    with Nd4jMatrixValue
                    with Nd4jLeaf
                    with StdMath

