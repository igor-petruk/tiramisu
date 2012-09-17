package org.tiramisu

import javax.servlet._
import javax.servlet.http._
import util.{RoutesTree}

class Tiramisu extends Filter with Controller with Compositing {
  val requestObject = new ThreadLocal[HttpServletRequest]
  val responseObject = new ThreadLocal[HttpServletResponse]
  val routeConfiguration = new ThreadLocal[RouteConfiguration]

  var routes = new RoutesTree

  def addRoute(newRoute:List[PathItem], handler:RouteHandler){
    routes.add(newRoute, handler)
  }

  def init(filterConfig: FilterConfig) {
  }

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    val req = request.asInstanceOf[HttpServletRequest]
    val stringPath = {
      val split = req.getServletPath.split("/").toList.tail
      if (req.getServletPath.endsWith("/"))
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
}
