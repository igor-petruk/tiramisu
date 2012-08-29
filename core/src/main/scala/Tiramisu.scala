package org.tiramisu

import javax.servlet._
import javax.servlet.http._
import scala.annotation.tailrec
import util.{RoutesTree, Tree}

class Tiramisu extends Filter with Controller {
  var requestObject = new ThreadLocal[HttpServletRequest]
  var responseObject = new ThreadLocal[HttpServletResponse]
  
  var routes = new RoutesTree

  def addRoute(newRoute:List[PathItem], handler:RouteHandler){
    routes.add(newRoute, handler)
  }

  def init(filterConfig: FilterConfig) {
  }

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    val req = request.asInstanceOf[HttpServletRequest]
    val stringPath = req.getServletPath.split("/").toList.tail
    val resultingHandler = routes.traverse(stringPath)
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
