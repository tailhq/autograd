package io.github.tailabs.autograd.graph

import scala.language.higherKinds


trait UnaryOp[U[_], T] extends Node[U, T]

trait BinaryOp[U[_], T] extends Node[U, T]
