package org.tiramisu.util

import scala.collection.mutable._
import annotation.tailrec
import org.tiramisu._
import java.io.{InputStream, OutputStream}
import scala.Some
import org.tiramisu.RouteHandler

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

  override def toString = "Tree[children=%s]".format(children)
}

class RoutesTree extends Tree[PathItem, RouteHandler]{

  def traverse(path:List[String])= traverseMe (path, this)

  def traverseDynamic(path:List[PathItem])= {
    val k = traverseMeDyn (path, Nil,this)
    k
  }

  @tailrec
  private def traverseMeDyn(path:List[PathItem], traversedPath:List[Tree[PathItem, RouteHandler]],
                            routesTree:Tree[PathItem, RouteHandler]):Option[RouteHandler]={
    // TODO: bad bug may be hidden here, also no support for ..
    println("dyn "+routesTree)
    path match{
      case Nil=>Option(routesTree.value).orElse(routesTree.children.get(StringPathItem.trailingSlash).map(_.value))
      case StringPathItem("..")::tail => traverseMeDyn(tail, traversedPath.tail, traversedPath.head)
      case head::tail => {
        routesTree.children.get(head) match {
          case Some(foundRoute) => traverseMeDyn(tail, routesTree::traversedPath, foundRoute)
          case None => routesTree.children.get(APathSpec[AnyRef]()).orElse(
            routesTree.children.get(MAPathSpec[AnyRef,Option]())) match {
            case Some(foundRoute) => traverseMeDyn(tail, routesTree::traversedPath,foundRoute)
            case None => None
          }
        }
      }
    }
  }

  @tailrec
  private def traverseMe(path:List[String], routesTree:Tree[PathItem, RouteHandler]):Option[RouteHandler]={
    println("nodyn "+path+"/"+routesTree)
    path match{
      case Nil=>Option(routesTree.value)
      case head::tail => {
        routesTree.children.get(StringPathItem(head)) match {
          case Some(foundRoute) => traverseMe(tail, foundRoute)
          case None => {
            routesTree.children.get(APathSpec[AnyRef]()).orElse(
            routesTree.children.get(MAPathSpec[AnyRef,Option]())) match {
            case Some(foundRoute) => {
              println("Found route "+foundRoute)
              traverseMe(tail, foundRoute)
            }
            case None => None
          }
          }
        }
      }
    }
  }


}

object IO{
  def copy(in: InputStream, out: OutputStream)={
    var count = 0L;
    var n = 0;
    val buf = new Array[Byte](4*1024)
    do{
      count += n
      out.write(buf,0,n)
      n = in.read(buf,0,buf.length)
    }while(n != -1)
    count;
  }
  
  implicit def closablePimp[T<:{def close()}](t:T) = new Traversable[T]{
    def foreach[U](f: (T) => U) {
      try{
        f(t)
      } finally {
        t.close()
      }
    }
  }
}