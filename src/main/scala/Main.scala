package com.igorpetruk.heroku.comet

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.annotations.AnnotationConfiguration
import org.eclipse.jetty.webapp._
import org.eclipse.jetty.plus.webapp.{PlusConfiguration, EnvConfiguration}

object Main {
  def main(args: Array[String]): Unit = {
  //  System.setProperty("org.eclipse.jetty.LEVEL", "DEBUG")
    val server = new Server(Integer.valueOf(System.getenv("PORT")))
    val webApp = new WebAppContext();
    webApp.setContextPath("/");
    webApp.setWar("src/main/webapp");
    webApp.setParentLoaderPriority(true);
    server.setHandler(webApp);
    server.start
    server.join
  }
}


