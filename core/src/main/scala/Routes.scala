package org.tiramisu

import org.tiramisu.providers._

import javax.servlet.http.HttpServletRequest
import scala.reflect.ClassTag
import annotation.tailrec

sealed class PathItem
case class StringPathItem(string:String) extends PathItem
case class TypedPathItem[T](types:List[Class[_]]) extends PathItem{
  var provider:EntityProvider[T] = null
}

sealed class PathSpec
case class StringPathSpec(string:String) extends PathSpec
abstract class TypedPathSpec[T] extends PathSpec{
  var provider:EntityProvider[T] = null
}
case class APathSpec[T](implicit t: ClassTag[T]) extends TypedPathSpec[T]{
  def aType = implicitly[ClassTag[T]].runtimeClass
  override def toString = "%s(%s)".format(this.getClass.getSimpleName,aType)
}
case class MAPathSpec[T,M[_]](implicit t: ClassTag[T], m: ClassTag[M[_]]) extends TypedPathSpec[M[T]]{
  def aType = implicitly[ClassTag[T]].runtimeClass
  def mType = implicitly[ClassTag[M[_]]].runtimeClass

  override def toString = "%s(%s)".format(this.getClass.getSimpleName,aType)
}

object StringPathItem{
  val trailingSlash = StringPathItem("")
}

case class RouteConfiguration(route:List[PathItem]=Nil)

case class RouteHandler(f:HttpServletRequest=>Unit, configuration:RouteConfiguration)

trait Route {
  var previous: Route = _
  var item: PathItem = _
  var servlet: Tiramisu = _

  def setup[Q <: Route, T](q: Q, other: Route, item: PathSpec, provider: EntityProvider[T]): Q = {
    q.previous = other
    item match {
      case t: TypedPathSpec[T] => t.provider = provider
      case _ =>
    }
    q.item = item match {
      case StringPathSpec(str) => StringPathItem(str)
      case i@APathSpec() => TypedPathItem[T](List(i.aType))
      case i@MAPathSpec() => TypedPathItem[T](List(i.mType, i.aType))
    }
    q.servlet = other.servlet
    q
  }

  object SimpleProvidedObject{
    def unapply(input:(String,_)):Option[Option[Any]]= input match {
      case (str,item@TypedPathItem(List(_))) => Some(Option(item.provider.provide(str)))
      case _ => None
    }
  }

  object SimpleOptionedObject{
    def unapply(input:(String,_)):Option[Option[Any]]= {
      println(input)
      val optClass = classOf[Option[_]]
      input match {
        case (str,item@TypedPathItem(List(`optClass`,_))) => Some(Option(item.provider.provide(str)))
        case _ => None
      }
    }
  }

  abstract sealed trait ResponseOutcome
  case class ErrorPage(errorCode:Int) extends ResponseOutcome
  case object Skip extends ResponseOutcome

  def parseRequest(path: List[PathItem], request: HttpServletRequest)(handle:List[Any]=>Unit){
    @tailrec
    def processItem(path:List[PathItem],stringPath:List[String],params:List[Any]){
      if (stringPath.isEmpty){
        handle(params.reverse)
      }else{
        val item = (stringPath.head, path.head) match {
          case SimpleProvidedObject(item) => item.toRight(ErrorPage(404))
          case SimpleOptionedObject(item) => Right(item)
          case (stringItem,StringPathItem(item)) if (stringItem==item) => Left(Skip)
          case _ => Left(ErrorPage(404))
        }
        item match {
          case Right(value) => processItem(path.tail, stringPath.tail, value::params)
          case Left(Skip) =>  processItem(path.tail, stringPath.tail, params)
          case Left(ErrorPage(number)) => {
            servlet.response.sendError(number)
          }
        }
      }
    }

    processItem(path, request.getServletPath.split("/").toList.tail, Nil)
  }

  def addRoute(handler:HttpServletRequest=>Unit){
    servlet.addRoute(path, RouteHandler(handler,
      servlet.syntacticScopeConfiguration.copy(route = path)
    ))
  }

  lazy val path = traverse.reverse

  private def traverse: List[PathItem] =
    if (previous != null)
      item :: previous.traverse
    else
      Nil
}

sealed trait SecurityOutcome
case object Accept extends SecurityOutcome
case object Reject extends SecurityOutcome

sealed trait Method
case object Post extends Method
case object Get extends Method

sealed trait OnResource
case object All extends OnResource
case class Item(id:String) extends OnResource

abstract class RestResource[T](implicit provider:EntityProvider[T]) extends Route0{
  def secure:PartialFunction[(Method,OnResource),SecurityOutcome]
}

class Route0 extends Route {
  def /(v: String) = setup(new Route0, this, StringPathSpec(v), StringDummyProvider)

  def /[T](v: APathSpec[T])(implicit runtimeClass: ClassTag[T], ep: EntityProvider[T]) =
    setup(new Route1[T], this, v, ep)

  def /[T](v: MAPathSpec[T,Option])(implicit runtimeClass: ClassTag[T], ep: EntityProvider[T]) =
    setup(new Route1[Option[T]], this, v, ep)

  def ->(f: => Unit) {
    def handler(h: HttpServletRequest) {
      f
    }
    addRoute(handler)
  }

  def ->[T] (r: RestResource[T]){

  }
}

class Route1[T1: ClassTag] extends Route {
  def /(v: String) = setup(new Route1[T1], this, StringPathSpec(v), StringDummyProvider)

  def /[T2](v: TypedPathSpec[T2])(implicit runtimeClass: ClassTag[T2], ep: EntityProvider[T2]) =
    setup(new Route2[T1, T2], this, v, ep)

  def ->(f: T1 => Unit) {
    def handler(h: HttpServletRequest) {
      parseRequest(path, h){parsedParams=>
        f(parsedParams(0).asInstanceOf[T1])
      }
    }
    addRoute(handler)
  }
}

class Route2[T1: ClassTag, T2: ClassTag] extends Route {
  def /(v: String) = setup(new Route2[T1, T2], this, StringPathSpec(v), StringDummyProvider)

  def /[T3](v: TypedPathSpec[T3])(implicit runtimeClass: ClassTag[T3], ep: EntityProvider[T3]) =
    setup(new Route3[T1, T2, T3], this, v, ep)

  def ->(f: (T1, T2) => Unit) {
    def handler(h: HttpServletRequest) {
      parseRequest(path, h){parsedParams=>
        f(
          parsedParams(0).asInstanceOf[T1],
          parsedParams(1).asInstanceOf[T2]
        )
      }
    }
    addRoute(handler)
  }
}

class Route3[T1: ClassTag, T2: ClassTag, T3: ClassTag] extends Route {
  def /(v: String) = setup(new Route3[T1, T2, T3], this, StringPathSpec(v), StringDummyProvider)

  def /[T4](v: TypedPathSpec[T4])(implicit runtimeClass: ClassTag[T4], ep: EntityProvider[T4]) =
    setup(new Route4[T1, T2, T3, T4], this, v, ep)

  def ->(f: (T1, T2, T3) => Unit) {
    def handler(h: HttpServletRequest) {
      parseRequest(path, h){parsedParams=>
        f(
          parsedParams(0).asInstanceOf[T1],
          parsedParams(1).asInstanceOf[T2],
          parsedParams(2).asInstanceOf[T3]
        )
      }
    }
    addRoute(handler)
  }
}

class Route4[T1, T2, T3, T4] extends Route {
  def /(v: String) = setup(new Route4[T1, T2, T3, T4], this, StringPathSpec(v), StringDummyProvider)

  def ->(f: (T1, T2, T3, T4) => Unit) {
    def handler(h: HttpServletRequest) {
      parseRequest(path, h){parsedParams=>
        f(
          parsedParams(0).asInstanceOf[T1],
          parsedParams(1).asInstanceOf[T2],
          parsedParams(2).asInstanceOf[T3],
          parsedParams(3).asInstanceOf[T4]
        )
      }
    }
    addRoute(handler)
  }
}


