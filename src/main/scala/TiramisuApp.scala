package org.tiramisu.app

import org.tiramisu._
import org.tiramisu.providers._
import akka.actor.{Props, Actor, ActorSystem}
import akka.pattern.ask
import scala.concurrent.util.duration._
import akka.util.Timeout
import concurrent.ExecutionContext
import ExecutionContext.Implicits.global

case class Book(name:String)

// Non intrusive controllers
trait IndexController{
  self:Controller with ProcessingService=>

  route /"index" -> view("index.jsp")

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

// May contain dependencies
trait BookController{
  self: Controller with BookRepository =>

  // This is automatic provider for entities from path params
  // Like /store/Amazon/books/232 will automatically fetch the book from dao
  implicit def bookProvider = booksDao

  // Example of how route prefix can be stored in variable
  val routeBooks = route /"store"/string/"books"/classOf[Book]

  // And then used
  routeBooks /"view" -> { (storeName, book)=>   // These params are type-safe, inferred from route
    println(storeName+"/"+book)
    view("book.jsp", Map("storeName"->storeName,"book"->book))
  }

  routeBooks /"json" -> { (_, book)=> json(book) }
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
