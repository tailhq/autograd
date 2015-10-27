package com.kogecoo.scalaad.breeze.test.helper.gen

import com.kogecoo.scalaad.test.helper.gen.{ContainerConstSample, VarSample, ScalarConstSample}
import org.scalacheck.Gen

import scala.language.higherKinds


class MMulShapeRestrictedNodePairGen[U[_], T](shape: MMulMatrixShapeGen, nodes: ShapeRestrictedNodeGen[U, T]) {

  type Restriction = T => Boolean

  def genSS(r1: Restriction, r2: Restriction): Gen[(ScalarConstSample[U, T], ScalarConstSample[U, T])] = {
    genSSWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genSC(r1: Restriction, r2: Restriction): Gen[(ScalarConstSample[U, T], ContainerConstSample[U, T])] = {
    genSCWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genSV(r1: Restriction, r2: Restriction): Gen[(ScalarConstSample[U, T], VarSample[U, T])] = {
    genSVWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genCS(r1: Restriction, r2: Restriction): Gen[(ContainerConstSample[U, T], ScalarConstSample[U, T])] = {
    genCSWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genCC(r1: Restriction, r2: Restriction): Gen[(ContainerConstSample[U, T], ContainerConstSample[U, T])] = {
    genCCWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genCV(r1: Restriction, r2: Restriction): Gen[(ContainerConstSample[U, T], VarSample[U, T])] = {
    genCVWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genVS(r1: Restriction, r2: Restriction): Gen[(VarSample[U, T], ScalarConstSample[U, T])] = {
    genVSWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genVC(r1: Restriction, r2: Restriction): Gen[(VarSample[U, T], ContainerConstSample[U, T])] = {
    genVCWithShape(r1, r2).map { g => (g._2, g._3) }
  }

  def genVV(r1: Restriction, r2: Restriction): Gen[(VarSample[U, T], VarSample[U, T])] = {
    genVVWithShape(r1, r2).map { g => (g._2, g._3) }
  }


  def genSSWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), ScalarConstSample[U, T], ScalarConstSample[U, T])] = {
    for {
      sc1 <- nodes.genScalarConstWithSource(r1)
      sc2 <- nodes.genScalarConstWithSource(r2)
    } yield ((MatrixShape(1, 1), MatrixShape(1, 1)), sc1, sc2)
  }

  def genSCWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), ScalarConstSample[U, T], ContainerConstSample[U, T])] = {
    for {
      (_, s2)  <- shape.genShape()
      sc       <- nodes.genScalarConstWithSource(r1)
      cc       <- nodes.genContainerConstWithSource(s2, r2)
    } yield ((MatrixShape(1, 1), s2), sc, cc)
  }

  def genSVWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), ScalarConstSample[U, T], VarSample[U, T])] = {
    for {
      (_, s2) <- shape.genShape()
      sc <- nodes.genScalarConstWithSource(r1)
      v  <- nodes.genVarWithSource(s2, r2)
    } yield ((MatrixShape(1, 1), s2), sc, v)
  }

  def genCSWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), ContainerConstSample[U, T], ScalarConstSample[U, T])] = {
    for {
      (s1, _) <- shape.genShape()
      cc      <- nodes.genContainerConstWithSource(s1, r1)
      sc      <- nodes.genScalarConstWithSource(r2)
    } yield ((s1, MatrixShape(1, 1)), cc, sc)
  }

  def genCCWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), ContainerConstSample[U, T], ContainerConstSample[U, T])] = {
    for {
      (s1, s2) <- shape.genShape()
      cc1      <- nodes.genContainerConstWithSource(s1, r1)
      cc2      <- nodes.genContainerConstWithSource(s2, r2)
    } yield ((s1, s2), cc1, cc2)
  }

  def genCVWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), ContainerConstSample[U, T], VarSample[U, T])] = {
    for {
      (s1, s2) <- shape.genShape()
      cc       <- nodes.genContainerConstWithSource(s1, r1)
      v        <- nodes.genVarWithSource(s2, r2)
    } yield ((s1, s2), cc, v)
  }

  def genVSWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), VarSample[U, T], ScalarConstSample[U, T])] = {
    for {
      (s1, _) <- shape.genShape()
      v  <- nodes.genVarWithSource(s1, r1)
      sc <- nodes.genScalarConstWithSource(r1)
    } yield ((s1, MatrixShape(1, 1)), v, sc)
  }

  def genVCWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), VarSample[U, T], ContainerConstSample[U, T])] = {
    for {
      (s1, s2) <- shape.genShape()
      v        <- nodes.genVarWithSource(s1, r1)
      cc       <- nodes.genContainerConstWithSource(s2, r2)
    } yield ((s1, s2), v, cc)
  }

  def genVVWithShape(r1: Restriction, r2: Restriction): Gen[((MatrixShape, MatrixShape), VarSample[U, T], VarSample[U, T])] = {
    for {
      (s1, s2) <- shape.genShape()
      v1       <- nodes.genVarWithSource(s1, r1)
      v2       <- nodes.genVarWithSource(s2, r2)
    } yield ((s1, s2), v1, v2)
  }

}
