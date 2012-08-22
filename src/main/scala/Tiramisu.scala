package org.tiramisu

import scala.reflect.ClassTag
import javax.servlet._
import javax.servlet.http._
import util.Tree
import scala.annotation.tailrec

object providers{

  implicit object StringDummyProvider extends EntityProvider[String]{
    def provide(str: String):String = str
  }

  implicit object IntProvider extends EntityProvider[Int]{
    def provide(id: String) = Integer.valueOf(id)
  }
}

import providers._

sealed class PathItem
case class StringPathItem(string:String) extends PathItem
case class TypedPathItem[T:ClassTag](optioned:Class[_]=null) extends PathItem{
  var provider:EntityProvider[T] = _
  def enclosedType = implicitly[ClassTag[T]].runtimeClass
  override def toString = "%s(%s)".format(this.getClass.getSimpleName,enclosedType+"/"+optioned)
}
case object StartItem extends PathItem

case class RouteHandler(f:HttpServletRequest=>Unit)

trait Route{
  var previous: Route = _
  var item: PathItem  = _
  var servlet:Tiramisu  = _

  def setup[Q<:Route,T](q:Q, other:Route, item:PathItem, provider:EntityProvider[T]):Q={
    q.previous = other
    item match {
      case t:TypedPathItem[T] => t.provider = provider
      case _ =>
    }
    q.item = item
    q.servlet = other.servlet
    q
  }

  def parseRequest(path:List[PathItem],request:HttpServletRequest):List[Any]={
    println("Parsing "+request.getServletPath+" for "+path)
    (request.getServletPath.split("/").toList.tail zip path) collect {
      case (str, item:TypedPathItem[_])=> item.provider.provide(str)
    } 
  }

  lazy val path = traverse.reverse

  private def traverse:List[PathItem]=
    if (previous!=null)
      item :: previous.traverse
    else
      Nil
}

class Route0 extends Route{
  def /(v:String) = setup(new Route0,this, StringPathItem(v),StringDummyProvider)
  def /[T](v:TypedPathItem[T])(implicit runtimeClass:ClassTag[T], ep:EntityProvider[T])=
    setup(new Route1[T],this, v, ep)
  def ->(f: =>Unit){
    def handler(h:HttpServletRequest){
      f
    }
    servlet.addRoute(path,RouteHandler(handler))
  }
}

class Route1[T1:ClassTag] extends Route{
  def /(v:String) = setup(new Route1[T1],this, StringPathItem(v),StringDummyProvider)
  def /[T2](v:TypedPathItem[T2])(implicit runtimeClass:ClassTag[T2], ep:EntityProvider[T2])=
    setup (new Route2[T1,T2],this, v, ep)
  def ->(f: T1=>Unit){
    def handler(h:HttpServletRequest){
      val parsedParams = parseRequest(path,h)
      f(parsedParams(0).asInstanceOf[T1])
    }
    servlet.addRoute(path,RouteHandler(handler))
  }
}

class Route2[T1:ClassTag,T2:ClassTag] extends Route{
  def /(v:String)= setup(new Route2[T1,T2],this, StringPathItem(v),StringDummyProvider)
  def /[T3](v:TypedPathItem[T3])(implicit runtimeClass:ClassTag[T3], ep:EntityProvider[T3])=
    setup(new Route3[T1,T2,T3],this, v, ep)
  def ->(f: (T1,T2)=>Unit){
    def handler(h:HttpServletRequest){
      val parsedParams = parseRequest(path, h)
      f(
        parsedParams(0).asInstanceOf[T1],
        parsedParams(1).asInstanceOf[T2]
      )
    }
    servlet.addRoute(path,RouteHandler(handler))
  }
}

class Route3[T1:ClassTag,T2:ClassTag,T3:ClassTag] extends Route{
  def /(v:String)= setup(new Route3[T1,T2,T3],this, StringPathItem(v),StringDummyProvider)
  def /[T4](v:TypedPathItem[T4])(implicit runtimeClass:ClassTag[T4], ep:EntityProvider[T4])=
    setup(new Route4[T1,T2,T3,T4],this, v, ep)
  def ->(f: (T1,T2,T3)=>Unit){
    def handler(h:HttpServletRequest){
      val parsedParams = parseRequest(path, h)
      f(
        parsedParams(0).asInstanceOf[T1],
        parsedParams(1).asInstanceOf[T2],
        parsedParams(2).asInstanceOf[T3]
      )
    }
    servlet.addRoute(path,RouteHandler(handler))
  }
}

class Route4[T1,T2,T3,T4] extends Route{
  def /(v:String) = setup(new Route4[T1,T2,T3,T4],this, StringPathItem(v),StringDummyProvider)
  def ->(f: (T1,T2,T3,T4)=>Unit){
    def handler(h:HttpServletRequest){
      val parsedParams = parseRequest(path, h)
      f(
        parsedParams(0).asInstanceOf[T1],
        parsedParams(1).asInstanceOf[T2],
        parsedParams(2).asInstanceOf[T3],
        parsedParams(3).asInstanceOf[T4]
      )
    }
    servlet.addRoute(path,RouteHandler(handler))
  }
}

@scala.annotation.implicitNotFound(msg =
  "No EntityProvider available for ${T}. Cannot convert path param to entity")
trait EntityProvider[T]{
  def provide(id:String):T
}

trait Controller{
  self:Tiramisu =>

  def jspPrefix = "/WEB-INF/jsp/"
  def jspSuffix = ".jsp"

  def request = requestObject.get()
  def response = responseObject.get()
  def out = response.getWriter

  def route = {
    val route = new Route0
    route.previous = null
    route.item = StartItem
    route.servlet = this
    route
  }
  val int = TypedPathItem[Int]()
  val string = TypedPathItem[String]()
  implicit def c2t[T](v:Class[T])(implicit t:ClassTag[T],p:EntityProvider[T]) = TypedPathItem[T]()
  def opt[T:ClassTag](v:Class[T])  = TypedPathItem[Option[T]](v)

  def view(jsp:String, params:AnyRef*){
    for (value<-params){
      value match {
        case (key:String, data)=> request.setAttribute(key,data)
        case other=> request.setAttribute(value.getClass.getSimpleName,value)
      }
    }
    request.getRequestDispatcher(jspPrefix+jsp+jspSuffix).forward(request, response)
  }

  def json(any:Any){}

  def async:AsyncContext = null
}

class Tiramisu extends Filter with Controller {
  var requestObject = new ThreadLocal[HttpServletRequest]
  var responseObject = new ThreadLocal[HttpServletResponse]
  
  var routes = new Tree[PathItem, RouteHandler]

  def addRoute(newRoute:List[PathItem], handler:RouteHandler){
    routes.add(newRoute, handler)
  }

  def init(filterConfig: FilterConfig) {
  }

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    val req = request.asInstanceOf[HttpServletRequest]
    val stringPath = req.getServletPath.split("/").toList.tail
    
    println("Handling "+stringPath)
    
    @tailrec
    def traverse(path:List[String],routesTree:Tree[PathItem, RouteHandler]):Option[RouteHandler]={
      path match{
        case Nil=>Option(routesTree.value)
        case head::tail => {
          routesTree.children.get(StringPathItem(head)) match {
            case Some(foundRoute) => traverse(tail, foundRoute)
            case None => routesTree.children.get(TypedPathItem[AnyRef]()) match {
              case Some(foundRoute) => traverse(tail, foundRoute)
              case None => None
            }
          }
        }
      }
    }
    
    val resultingHandler = traverse(stringPath, routes)
    resultingHandler match {
      case Some(handler)=>{
        requestObject.set(req)
        responseObject.set(response.asInstanceOf[HttpServletResponse])
        handler.f(req)
      }
      case None => chain.doFilter(request, response)
    }
    
  }

  def destroy() {}
}
