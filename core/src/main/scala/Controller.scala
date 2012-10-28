package org.tiramisu

import reflect.ClassTag
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.tiramisu.util.RoutesTree
import javax.servlet.{FilterChain, ServletResponse, ServletRequest, FilterConfig}

trait Controller extends Compositing{
  self:Tiramisu =>

  def jspPrefix = "/WEB-INF/jsp/"
  def jspSuffix = ".jsp"

  val requestObject = new ThreadLocal[HttpServletRequest]
  val responseObject = new ThreadLocal[HttpServletResponse]
  val routeConfiguration = new ThreadLocal[RouteConfiguration]

//  def noTemplateRequest = request.getHeader("X-Tiramisu-Template")=="false"
  def noTemplateRequest = request.getParameter("tiramisuajax")=="1"

  var routes = new RoutesTree

  var filterConfig:FilterConfig = _

  def addRoute(newRoute:List[PathItem], handler:RouteHandler){
    routes.add(newRoute, handler)
  }

  def init(filterConfig: FilterConfig) {
	this.filterConfig = filterConfig
  }

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    val req = request.asInstanceOf[HttpServletRequest]
    val stringPath = {
      val path = req.getServletPath
      val split = if (path=="/") List() else req.getServletPath.split("/").toList.tail
      if (path!="/" && req.getServletPath.endsWith("/"))
        split:::""::Nil
      else
        split
    }
    val resultingHandler = routes.traverse(stringPath)
    resultingHandler match {
      case Some(handler)=>{
        requestObject.set(req)
        responseObject.set(response.asInstanceOf[HttpServletResponse])
        routeConfiguration.set(handler.configuration)
        handler.f(req)
      }
      case None => chain.doFilter(request, response)
    }

  }

  def destroy() {}

  def request = requestObject.get()
  def response = responseObject.get()
  def out = response.getWriter

  def route = {
    val route = new Route0
    route.previous = null
    route.item = null
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
}

