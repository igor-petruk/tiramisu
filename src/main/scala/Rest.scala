package com.igorpetruk.heroku.comet.rest

import javax.ws.rs.{Produces, GET, Path}
import beans.BeanProperty
import javax.ws.rs.core.MediaType
import com.sun.jersey.api.view.Viewable
import java.util.Date

class MyClass(fSomeValue:String){
  @BeanProperty
  var someValue:String = fSomeValue
}

@Path("/test")
class TestResource {
  @GET
  @Path("/json")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def test() = new MyClass("test")

  @GET
  @Path("/page")
  @Produces(Array(MediaType.TEXT_HTML))
  def page() = new Viewable("/index.jsp",new Date)

}
