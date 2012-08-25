package org.tiramisu

@scala.annotation.implicitNotFound(msg =
  "No EntityProvider available for ${T}. Cannot convert path param to entity")
trait EntityProvider[T]{
  def provide(id:String):T
}

object providers{

  implicit object StringDummyProvider extends EntityProvider[String]{
    def provide(str: String):String = str
  }

  implicit object IntProvider extends EntityProvider[Int]{
    def provide(id: String) = Integer.valueOf(id)
  }
}

