package com.kogecoo.scalaad.impl.breeze

import com.kogecoo.scalaad.impl.std.{StdScalarEval, StdScalarValue}


object Implicits extends StdScalarEval
                    with StdScalarValue
                    with BreezeVectorEval
                    with BreezeVectorValue
                    with BreezeMatrixEval
                    with BreezeMatrixValue
                    with BreezeLeaf
                    with StdMath

