package org.tiramisu

import scala.reflect.ClassTag
import javax.servlet._
import javax.servlet.http._

sealed class PathItem
case class StringPathItem(string:String) extends PathItem
case class TypedPathItem[T:ClassTag](optioned:Class[_]=null) extends PathItem{
  override def toString = "%s(%s)".format(this.getClass.getSimpleName,implicitly[ClassTag[T]].runtimeClass+"/"+optioned)
}
case object StartItem extends PathItem

trait Route{
  def previous: Route
  def item: PathItem

  lazy val path = traverse.reverse

  private def traverse:List[PathItem]=
    if (previous!=null)
      item :: previous.traverse
    else
      Nil
}

class Route0(prev:Route=null, i:PathItem = StartItem) extends Route{
  def previous = prev
  def item = i
  def /(v:String) = new Route0(this, StringPathItem(v))
  def /[T :ClassTag](v:TypedPathItem[T])=new Route1[T](this, v)
  def ->(f: =>Unit){

  }
}

class Route1[T1:ClassTag](prev:Route, i:PathItem) extends Route{
  def previous = prev
  def item = i
  def /(v:String) = new Route1[T1](this, StringPathItem(v))
  def /[T2:ClassTag](v:TypedPathItem[T2])=new Route2[T1,T2](this, v)
  def ->(f: T1=>Unit){

  }
}

class Route2[T1:ClassTag,T2:ClassTag](prev:Route, i:PathItem) extends Route{
  def previous = prev
  def item = i
  def /(v:String)= new Route2[T1,T2](this, StringPathItem(v))
  def /[T3 :ClassTag](v:TypedPathItem[T3])=new Route3[T1,T2,T3](this, v)
  def ->(f: (T1,T2)=>Unit){
    println(path)
  }
}

class Route3[T1:ClassTag,T2:ClassTag,T3:ClassTag](prev:Route, i:PathItem) extends Route{
  def previous = prev
  def item = i
  def /(v:String)= new Route3[T1,T2,T3](this, StringPathItem(v))
  def /[T4 :ClassTag](v:TypedPathItem[T4])=new Route4[T1,T2,T3,T4](this, v)
  def ->(f: (T1,T2,T3)=>Unit){
    println(path)
  }
}

class Route4[T1,T2,T3,T4](prev:Route, i:PathItem) extends Route{
  def previous = prev
  def item = i
  def /(v:String) = new Route4(this, StringPathItem(v))
}

trait EntityProvider[T]{
  def provide(id:String):T
}

trait Controller{
  def route = new Route0
  val int = TypedPathItem[Int]()
  val string = TypedPathItem[String]()
  implicit def c2t[T](v:Class[T])(implicit t:ClassTag[T],p:EntityProvider[T]) = TypedPathItem[T]()
  def opt[T:ClassTag](v:Class[T])  = TypedPathItem[Option[T]](v)

  def view(name:String, params:Map[String,Any]=Map()){}
  def json(any:Any){}

  def async:AsyncContext = null
}

class Tiramisu extends Filter with Controller {

  def init(filterConfig: FilterConfig) {
    val klass = this.getClass
    for (f<-klass.getDeclaredFields){
      println(f.getName+" "+f.getType)
    }
  }

  def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain) {
    val req = request.asInstanceOf[HttpServletRequest]
    println(req.getServletPath)
    if (req.getServletPath.startsWith("/some")){
      val k = req.startAsync()
    }else{
      chain.doFilter(request, response)
    }
  }

  def destroy() {}
}

