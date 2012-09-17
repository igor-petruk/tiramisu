package org.tiramisu

import scala.xml.NodeSeq

import org.tiramisu.providers._
import org.tiramisu.util.IO._

trait ResourceProvider {
  def resource(version: Option[String]): NodeSeq
  
  def depends:List[String] = List()
}

trait Resources { self: Controller =>
  val servePrefix = "tiramisuServe"
  val resourcePrefix = "/serve"
  
  route /servePrefix/string -> {resourceName=>
    for (resource<-getClass.getResourceAsStream(resourcePrefix+"/"+resourceName)){
      copy(resource, response.getOutputStream)
    }
  }
  
  class TiramisuJS extends ResourceProvider {
    def resource(version: Option[String]) =
      <script src={ "/"+servePrefix +"/tiramisu.js" }></script>

    override def depends = List("jquery")
  }

  class JQueryJS extends ResourceProvider {
    def resource(version: Option[String]) =
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.js"></script>
  }

  val resourcesMap = Map[String, ResourceProvider](
    "tiramisu" -> new TiramisuJS,
    "jquery" -> new JQueryJS
  )
}
