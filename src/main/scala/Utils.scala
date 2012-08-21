package org.tiramisu.util

import scala.collection.mutable._
import annotation.tailrec

class Tree[K, V] {

  val children = Map[K, Tree[K,V]]()
  var value: V = _

  @tailrec
  final def add(path: List[K], value: V) {
    path match {
      case head :: tail => {
        val subtree = children.getOrElseUpdate(head, new Tree[K, V]())
        subtree.add(tail, value)
      }
      case Nil => this.value = value
    }
  }

}

