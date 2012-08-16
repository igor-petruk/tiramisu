package org.tiramisu.app

import org.tiramisu._

case class Book(name:String)

// Non intrusive controllers
trait IndexController{self:Controller=>
  route /"index" -> View("index.jsp")
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
    View("book.jsp", Map("storeName"->storeName,"book"->book))
  }

  routeBooks /"json" -> { (_, book)=> Json(book) }
}

trait BookRepository{
  lazy val booksDao = new EntityProvider[Book]{
    def provide(id:String) = Book("Book"+id)
  }
}

class BookApplication extends Tiramisu
  with BookRepository
  with IndexController
  with BookController
