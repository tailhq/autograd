package io.github.tailabs.autograd.test.helper.arbitrary

import io.github.tailabs.autograd.test.helper.gen.{ScalarIntValueGen, ScalarIntNodeGen}
import io.github.tailabs.autograd.graph._
import io.github.tailabs.autograd.value.ContainerValue
import io.github.tailabs.autograd.value.{ContainerValue, NonContainerValue, Value}

import org.scalacheck.Arbitrary


object ArbitraryScalarInt {

  type T = Node[Scalar, Int]

  private[this] val nodeGen = new ScalarIntNodeGen

  private[this] val valueGen = new ScalarIntValueGen


  object Implicits {

    implicit def arbNode(implicit a: Arbitrary[Node[Scalar, Int]]): Arbitrary[Node[Scalar, Int]] = Arbitrary {
      nodeGen.genNode()
    }

    implicit def arbVar(implicit a: Arbitrary[Var[Scalar, Int]]): Arbitrary[Var[Scalar, Int]] = Arbitrary {
      nodeGen.genVar()
    }

    implicit def arbScalarConst(implicit a: Arbitrary[ScalarConst[Scalar, Int]]): Arbitrary[ScalarConst[Scalar, Int]] = Arbitrary {
      nodeGen.genScalarConst()
    }

    implicit def arbContainerConst(implicit a: Arbitrary[ContainerConst[Scalar, Int]]): Arbitrary[ContainerConst[Scalar, Int]] = Arbitrary {
      nodeGen.genContainerConst()
    }

    implicit def arbValue(implicit a: Arbitrary[Value[Scalar, Int]]): Arbitrary[Value[Scalar, Int]] = Arbitrary {
      valueGen.genValue()
    }

    implicit def arbNonContainerValue(implicit a: Arbitrary[Value[Scalar, Int]]): Arbitrary[NonContainerValue[Scalar, Int]] = Arbitrary {
      valueGen.genNonContainerValue()
    }

    implicit def arbContainerValue(implicit a: Arbitrary[Value[Scalar, Int]]): Arbitrary[ContainerValue[Scalar, Int]] = Arbitrary {
      valueGen.genContainerValue()
    }
  }

}
