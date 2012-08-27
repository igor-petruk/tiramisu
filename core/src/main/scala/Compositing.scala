package org.tiramisu

import xml._


trait Tag{
  def name:String

  implicit def nodePimp(node:Node)=new{
    def attributeAsText(key:String):Option[String]=node.attribute(key).map(_.text)
  }

  def transform(elem:Elem):Option[NodeSeq]
}

case class TagDescriptor(namespace:String, tags:Seq[Tag])


trait Compositing{ self:Controller=>
  def tiraviewPrefix = "/WEB-INF/tiraview/"
  def tiraviewSuffix = ".xhtml"

  def processTag(node:Elem):Option[NodeSeq]={
    val proc = node match {
      case someElem:Elem if (descriptors.contains(someElem.namespace)) =>{
        val desc = descriptors(someElem.namespace).get(someElem.label)
        for (descFound <- desc; processed <- descFound.transform(node)) yield processed
      }
      case _=>None
    }
    proc
  }

  def processTags(node:Node):NodeSeq={
    node match {
      case elem:Elem =>  {
        val p = processTag(elem) match {
          case Some(newTag) => newTag
          case None => elem
        }
        val result = p match {
          case someElem:Elem => {
            val processedChildren = someElem.child.flatMap(processTags(_))
            import someElem._
            Elem(prefix,label,someElem.attributes,
              /*TODO: Not topscope!!!*/TopScope,
              minimizeEmpty, processedChildren.toArray:_*)
          }
          case _ => p.flatMap(processTags(_))
        }
        result
      }
      case _ => node
    }
  }

  val tContent = new Tag {
    def name = "content"

    def transform(elem: Elem) = {
      val page = tComposite.currentPage.get
      val data = (for (dataItem <- page \\ "data"
                       if (dataItem.namespace == elem.namespace)
                         && (dataItem.attributeAsText("name")==elem.attributeAsText("name"))
      ) yield dataItem).head

      val m = data match {
        case someElem:Elem => {
          someElem.child
        }
        case _ => elem.child
      }
      Some(m)
    }

  }

  val tComposite = new Tag{
    def name = "composite"

    val currentPage = new ThreadLocal[Elem]

    def transform(elem: Elem) = {
      currentPage.set(elem)
      for (templateName <- elem.attributeAsText("template");
           template <- getTemplate(templateName)) yield template
    }
    
    def getTemplate(name:String):Option[Elem]=Some(loadXml(name))
  }
  
  def loadXml(name:String)={
    val xmlResourse = tiraviewPrefix+name+tiraviewSuffix;
    println("Loading "+xmlResourse)
    XML.load(getClass.getResource(xmlResourse))
  }

  def toDescriptorMap(list:TagDescriptor*)=list.groupBy(_.namespace).map{item=>
    (
      item._1,
      item._2.flatMap(descriptor=>descriptor.tags.groupBy(_.name)).toMap.map(item1=>
        (item1._1, item1._2.head)
      )
      )
  }

  val descriptors = toDescriptorMap(
    TagDescriptor("http://tiramisu.org/dev-0", List(tComposite, tContent))
  )

  def compose(pageName:String){
    val page = loadXml(pageName)
    val processed = processTags(page)
    response.setContentType("text/html")//; charset=utf-8")
    response.setCharacterEncoding("utf-8")
    out.println("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    out.println(processed)
  }
}
