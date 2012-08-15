package com.igorpetruk.heroku.comet

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHolder, ServletContextHandler}
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import com.sun.jersey.spi.container.servlet.ServletContainer
import com.sun.jersey.api.json.JSONConfiguration
import java.util.logging.{Level, Logger}
import org.eclipse.jetty.webapp.WebAppContext

object Main {
  def main(args: Array[String]): Unit = {
    System.setProperty("org.eclipse.jetty.LEVEL", "DEBUG")
    val server = new Server(Integer.valueOf(System.getenv("PORT")))
    val webApp = new WebAppContext();
    webApp.setContextPath("/");
    webApp.setWar("src/main/webapp");
    server.setHandler(webApp);
    server.start
    server.join
  }
}

class Main extends HttpServlet {
  protected override def doGet(req: HttpServletRequest, resp: HttpServletResponse): Unit = {
    resp.getWriter.print("Hello my Rocksy!\n")
  }
}


