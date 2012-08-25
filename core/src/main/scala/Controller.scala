package org.tiramisu

import reflect.ClassTag

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

