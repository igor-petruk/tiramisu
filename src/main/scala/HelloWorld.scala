package com.igorpetruk.heroku.comet

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val server = new Server(Integer.valueOf(System.getenv("PORT")))
    val context = new ServletContextHandler(ServletContextHandler.SESSIONS)
    context.setContextPath("/")
    server.setHandler(context)
    context.addServlet(new ServletHolder(new HelloWorld), "/*")
    server.start
    server.join
  }
}

class HelloWorld extends HttpServlet {
  protected override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    resp.getWriter.print("Hello my Rocksy!\n")
  }
}


