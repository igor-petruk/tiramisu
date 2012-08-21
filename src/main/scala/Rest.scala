package com.igorpetruk.heroku.comet.rest

import javax.ws.rs.{Produces, GET, Path}
import beans.BeanProperty
import com.sun.jersey.api.view.Viewable
import java.util.Date
import javax.ws.rs.core.{Context, MediaType}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}


class MyClass(fSomeValue:String){
  @BeanProperty
  var someValue:String = fSomeValue
}

@Path("/test")
class TestResource {
  @GET
  @Path("/async")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def async(@Context req:HttpServletRequest, @Context resp:HttpServletResponse){
    /*val asyncContext = req.startAsync(req, resp);
    val f = future{
      Thread.sleep(5000)
      val o = new ObjectMapper
      o.writeValue(asyncContext.getResponse.getOutputStream,new MyClass("async "+new Date) )
      asyncContext.complete();
    } */
  }

  @GET
  @Path("/json")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def test() = new MyClass("test")

  @GET
  @Path("/page")
  @Produces(Array(MediaType.TEXT_HTML))
  def page() = new Viewable("/index.jsp",new Date)

}
