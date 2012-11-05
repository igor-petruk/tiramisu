package org.tiramisu

import reflect.ClassTag

@scala.annotation.implicitNotFound(msg =
  "No EntityProvider available for ${T}. Cannot convert path param to entity")
trait EntityProvider[T]{
  def provide(id:String):T
}

trait EntityProvision{
  val int = APathSpec[Int]()
  val string = APathSpec[String]()
  implicit def c2t[T](v:Class[T])(implicit t:ClassTag[T],p:EntityProvider[T]) = APathSpec[T]()
  def opt[T:ClassTag](v:Class[T])  = MAPathSpec[T,Option]()
}

object providers{

  implicit object StringDummyProvider extends EntityProvider[String]{
    def provide(str: String):String = str
  }

  implicit object IntProvider extends EntityProvider[Int]{
    def provide(id: String) = Integer.valueOf(id)
  }
}

