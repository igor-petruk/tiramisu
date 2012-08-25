package org.tiramisu.util

import scala.collection.mutable._
import annotation.tailrec
import org.tiramisu.StringPathItem._
import org.tiramisu.TypedPathItem._
import org.tiramisu.{TypedPathItem, StringPathItem, RouteHandler, PathItem}

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

class RoutesTree extends Tree[PathItem, RouteHandler]{

  def traverse(path:List[String])= traverseMe (path, this)

  @tailrec
  private def traverseMe(path:List[String], routesTree:Tree[PathItem, RouteHandler]):Option[RouteHandler]={
    println("Trav:" + path)
    path match{
      case Nil=>Option(routesTree.value)
      case head::tail => {
        routesTree.children.get(StringPathItem(head)) match {
          case Some(foundRoute) => traverseMe(tail, foundRoute)
          case None => routesTree.children.get(TypedPathItem[AnyRef]()) match {
            case Some(foundRoute) => traverseMe(tail, foundRoute)
            case None => None
          }
        }
      }
    }
  }


}
