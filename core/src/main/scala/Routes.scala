package org.tiramisu

import org.tiramisu.providers._

import javax.servlet.http.HttpServletRequest
import scala.reflect.ClassTag

sealed class PathItem
case class StringPathItem(string:String) extends PathItem
case class TypedPathItem[T:ClassTag](optioned:Class[_]=null) extends PathItem{
  var provider:EntityProvider[T] = _
  def enclosedType = implicitly[ClassTag[T]].runtimeClass
  override def toString = "%s(%s)".format(this.getClass.getSimpleName,enclosedType+"/"+optioned)
}

case class RouteHandler(f:HttpServletRequest=>Unit)

trait Route {
  var previous: Route = _
  var item: PathItem = _
  var servlet: Tiramisu = _

  def setup[Q <: Route, T](q: Q, other: Route, item: PathItem, provider: EntityProvider[T]): Q = {
    q.previous = other
    item match {
      case t: TypedPathItem[T] => t.provider = provider
      case _ =>
    }
    q.item = item
    q.servlet = other.servlet
    q
  }

  def parseRequest(path: List[PathItem], request: HttpServletRequest): List[Any] = {
    println("Parsing " + request.getServletPath + " for " + path)
    (request.getServletPath.split("/").toList.tail zip path) collect {
      case (str, item: TypedPathItem[_]) => item.provider.provide(str)
    }
  }

  lazy val path = traverse.reverse

  private def traverse: List[PathItem] =
    if (previous != null)
      item :: previous.traverse
    else
      Nil
}

class Route0 extends Route {
  def /(v: String) = setup(new Route0, this, StringPathItem(v), StringDummyProvider)

  def /[T](v: TypedPathItem[T])(implicit runtimeClass: ClassTag[T], ep: EntityProvider[T]) =
    setup(new Route1[T], this, v, ep)

  def ->(f: => Unit) {
    def handler(h: HttpServletRequest) {
      f
    }
    servlet.addRoute(path, RouteHandler(handler))
  }
}

class Route1[T1: ClassTag] extends Route {
  def /(v: String) = setup(new Route1[T1], this, StringPathItem(v), StringDummyProvider)

  def /[T2](v: TypedPathItem[T2])(implicit runtimeClass: ClassTag[T2], ep: EntityProvider[T2]) =
    setup(new Route2[T1, T2], this, v, ep)

  def ->(f: T1 => Unit) {
    def handler(h: HttpServletRequest) {
      val parsedParams = parseRequest(path, h)
      f(parsedParams(0).asInstanceOf[T1])
    }
    servlet.addRoute(path, RouteHandler(handler))
  }
}

class Route2[T1: ClassTag, T2: ClassTag] extends Route {
  def /(v: String) = setup(new Route2[T1, T2], this, StringPathItem(v), StringDummyProvider)

  def /[T3](v: TypedPathItem[T3])(implicit runtimeClass: ClassTag[T3], ep: EntityProvider[T3]) =
    setup(new Route3[T1, T2, T3], this, v, ep)

  def ->(f: (T1, T2) => Unit) {
    def handler(h: HttpServletRequest) {
      val parsedParams = parseRequest(path, h)
      f(
        parsedParams(0).asInstanceOf[T1],
        parsedParams(1).asInstanceOf[T2]
      )
    }
    servlet.addRoute(path, RouteHandler(handler))
  }
}

class Route3[T1: ClassTag, T2: ClassTag, T3: ClassTag] extends Route {
  def /(v: String) = setup(new Route3[T1, T2, T3], this, StringPathItem(v), StringDummyProvider)

  def /[T4](v: TypedPathItem[T4])(implicit runtimeClass: ClassTag[T4], ep: EntityProvider[T4]) =
    setup(new Route4[T1, T2, T3, T4], this, v, ep)

  def ->(f: (T1, T2, T3) => Unit) {
    def handler(h: HttpServletRequest) {
      val parsedParams = parseRequest(path, h)
      f(
        parsedParams(0).asInstanceOf[T1],
        parsedParams(1).asInstanceOf[T2],
        parsedParams(2).asInstanceOf[T3]
      )
    }
    servlet.addRoute(path, RouteHandler(handler))
  }
}

class Route4[T1, T2, T3, T4] extends Route {
  def /(v: String) = setup(new Route4[T1, T2, T3, T4], this, StringPathItem(v), StringDummyProvider)

  def ->(f: (T1, T2, T3, T4) => Unit) {
    def handler(h: HttpServletRequest) {
      val parsedParams = parseRequest(path, h)
      f(
        parsedParams(0).asInstanceOf[T1],
        parsedParams(1).asInstanceOf[T2],
        parsedParams(2).asInstanceOf[T3],
        parsedParams(3).asInstanceOf[T4]
      )
    }
    servlet.addRoute(path, RouteHandler(handler))
  }
}


