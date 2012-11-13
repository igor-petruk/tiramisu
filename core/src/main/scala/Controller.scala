package org.tiramisu

import reflect.ClassTag
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import org.tiramisu.util.RoutesTree
import javax.servlet.{FilterChain, ServletResponse, ServletRequest, FilterConfig}

trait JSP{ self:Controller =>
  def jspPrefix = "/WEB-INF/jsp/"
  def jspSuffix = ".jsp"

  def view(jsp:String, params:AnyRef*){
    for (value<-params){
      value match {
        case (key:String, data)=> request.setAttribute(key,data)
        case other=> request.setAttribute(value.getClass.getSimpleName,value)
      }
    }
    request.getRequestDispatcher(jspPrefix+jsp+jspSuffix).forward(request, response)
  }
}

trait Controller extends Compositing with EntityProvision with SessionBeans with RequestBeans{
  self:Tiramisu =>

  val requestObject = new ThreadLocal[HttpServletRequest]
  val responseObject = new ThreadLocal[HttpServletResponse]
  val routeConfiguration = new ThreadLocal[RouteConfiguration]

  var routes = new RoutesTree

  var filterConfig:FilterConfig = _

  def addRoute(newRoute:List[PathItem], handler:RouteHandler){
    routes.add(newRoute, handler)
  }

  def postprocessBeans{
     for (field<-self.getClass.getDeclaredFields){
       if (field.getType==classOf[Bean[Any]]){
         field.setAccessible(true)
         val bean = field.get(self).asInstanceOf[Bean[Any]]
         bean.beanId = field.getName
       }
     }
  }

  def init(filterConfig: FilterConfig) {
  	this.filterConfig = filterConfig
    postprocessBeans
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

}

