package org.tiramisu.app

import org.tiramisu._
import org.tiramisu.providers._
import akka.actor.{Props, Actor, ActorSystem}
import akka.pattern.ask
import scala.concurrent.util.duration._
import akka.util.Timeout
import concurrent.ExecutionContext
import ExecutionContext.Implicits.global
import java.util.Date

case class Book(name:String)

trait IndexController{self:Controller=>
  route /"index" -> view("index")
}

trait BookController{ self: Controller with BookRepository =>

  implicit def bookProvider = booksDao

  route /"store"/string/"books"/classOf[Book]/"view" -> { (storeName, book)=>
      view("index", book, "store"->storeName, new Date)
  }
}

trait AkkaController{
  self: Controller with ProcessingService =>

  route /"akka" -> {
    val request = async
    implicit val timeout = Timeout(5 seconds)
    (waitActor ? "Hello")
      .onComplete{ reply=>
      reply match {
        case Left(error) => view("results.jsp",Map("error"->error))
        case Right(response)=> view("results.jsp",Map("response"->response))
      }
      request.complete
    }
  }
}

trait ProcessingService{
  val actorSystem = ActorSystem("bookApp")

  val waitActor = actorSystem.actorOf(Props(new WaitActor))

  class WaitActor extends Actor{
    def receive = {
      case x => {
        Thread.sleep(1000)
        sender ! x
      }
    }
  }
}

trait BookRepository{
  lazy val booksDao = new EntityProvider[Book]{
    def provide(id:String) = Book("Book"+id)
  }
}

class BookApplication extends Tiramisu
  with BookRepository
  with ProcessingService
  with IndexController
  with BookController
  with AkkaController
