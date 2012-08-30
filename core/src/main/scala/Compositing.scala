package org.tiramisu

import xml._
import dtd.{DocType, DTD}
import factory.XMLLoader
import parsing.{XhtmlParser, ConstructingParser}
import scala.collection.concurrent.TrieMap
import javax.xml.parsers.SAXParserFactory
import java.net.URL
import org.apache.commons.jexl2.{JexlContext, JexlEngine, Expression}

trait Tag{
  def name:String

  implicit def nodePimp(node:Node)=new{
    def attributeAsText(key:String):Option[String]=node.attribute(key).map(_.text)
  }

  def transform(elem:Elem, context:PageContext):(NodeSeq,PageContext)
}

case class TagDescriptor(namespace:String, tags:Seq[Tag])

case class PageContext(map:Map[String,AnyRef]){
  def withItem(key:String, value:AnyRef) = PageContext(map + (key->value))
}

trait Compositing{ self:Controller=>
  def tiraviewPrefix = "/WEB-INF/tiraview/"
  def tiraviewSuffix = ".xhtml"

  def processTag(node:Elem, context: PageContext):(NodeSeq,PageContext)={
    node match {
      case someElem:Elem if (descriptors.contains(someElem.namespace)) =>{
        descriptors(someElem.namespace).get(someElem.label) match {
          case Some(descriptor) =>  descriptor.transform(node, context)
          case _ => sys.error("No tag "+someElem.label)
        }
      }
      case _=>(node, context)
    }
  }

  def processChildren(children:Seq[Node], startState:PageContext)={
    val processedCh = children.foldLeft((List[Node](),startState)){(acc, node)=>
      val processed = processTags(node, acc._2)
      (acc._1 ++ processed._1, processed._2)
    }
    (NodeSeq.fromSeq(processedCh._1), processedCh._2)
  }
  
  def processTags(node:Node, context:PageContext):(NodeSeq,PageContext)={
    node match {
      case elem:Elem =>  {
        //println("Processing elem: "+elem)
        val q@(processedElem, startContext) = processTag(elem, context)
        //println("Processed tag: "+q)
        processedElem match {
          case someElem:Elem => {

            val (processedChildren,finalContext) = processChildren(someElem.child, startContext)
            import someElem._
            val finalElem = Elem(prefix,label,someElem.attributes,
              /*TODO: Not topscope!!!*/TopScope,
              false, processedChildren.toArray:_*)
            (finalElem, finalContext)
          }
          case _ => processChildren(processedElem,startContext)
        }
      }
      case _ => (node,context)
    }
  }

  val tContent = new Tag {
    def name = "content"

    def transform(elem: Elem, context: PageContext) =  {
      val page = context.map("currentPage").asInstanceOf[Elem]
      val data = (for (dataItem <- page \\ "data"
                       if (dataItem.namespace == elem.namespace)
                         && (dataItem.attributeAsText("name")==elem.attributeAsText("name"))
      ) yield dataItem).head

      data match {
        case someElem:Elem => (someElem.child,context)
        case _ => (elem.child,context)
      }
    }

  }

  val tComposite = new Tag{
    def name = "composite"

    def transform(elem: Elem, context: PageContext) = {
      val template = for (templateName <- elem.attributeAsText("template");
           template <- getTemplate(templateName)) yield template
      template match {
        case Some(document) => (
          document.children,
          context withItem ("currentPage",elem)
                  withItem ("dtd",document.dtd)
        )
        case _ => sys.error("")
      }
    }

    def getTemplate(name:String):Option[Document]={
      val xml = loadXml(name)
      //println(xml)
      Some(xml)
    }
  }

  val tOut = new Tag{
    def name: String = "out"

    val jexlEngine = new JexlEngine()
    val cache = new TrieMap[String, Expression]()

    class PageJexlContext(context:PageContext) extends JexlContext {
      def get(name: String): AnyRef = context.map(name)

      def set(name: String, value: AnyRef) {}

      def has(name: String): Boolean = context.map.contains(name)
    }

    def transform(elem: Elem, context: PageContext): (NodeSeq, PageContext) = {
      val expressionText = elem.attributeAsText("value").getOrElse("");
      val expression = cache.getOrElseUpdate(expressionText,jexlEngine.createExpression(expressionText));
      val text=expression.evaluate(new PageJexlContext(context)).toString
      (Text(text),context)
    }
  }

  val loadedXmls = scala.collection.concurrent.TrieMap[String,Document]()
  
  def loadXml(name:String):Document=loadedXmls.getOrElseUpdate(name,{

    val xmlResourse = tiraviewPrefix+name+tiraviewSuffix;
    //println("Loading "+xmlResourse)
    val parser = scala.xml.parsing.ConstructingParser.fromSource(io.Source.fromURL(getClass.getResource(xmlResourse)), true)
    val doc = parser.document()
    doc
  })

  def toDescriptorMap(list:TagDescriptor*)=list.groupBy(_.namespace).map{item=>
    (
      item._1,
      item._2.flatMap(descriptor=>descriptor.tags.groupBy(_.name)).toMap.map(item1=>
        (item1._1, item1._2.head)
      )
      )
  }

  val descriptors = toDescriptorMap(
    TagDescriptor("http://tiramisu.org/dev-0", List(tComposite, tContent,tOut))
  )

  def compose(pageName:String, params:AnyRef*){
    val page = loadXml(pageName)

    val map = (for (value<-params) yield
      value match {
        case (key:String, data:AnyRef)=> (key->data)
        case other=> (value.getClass.getSimpleName->value)
      }
    ).toMap[String,AnyRef]

    val (finalPage, finalContext) = processTags(page.docElem, PageContext(map))
    response.setContentType("text/html")//; charset=utf-8")
    response.setCharacterEncoding("utf-8")
    val dtd = finalContext.map("dtd").asInstanceOf[DTD]
    out.print(
        "<!DOCTYPE html %s%s>".format(
          Option(dtd.externalID) getOrElse "",
          dtd.decls.mkString("", "\n", "")
        )
      )
    XML.write(out, finalPage(1),"utf-8",false, DocType("html",dtd.externalID,dtd.decls))
    //out.println(finalPage)
  }
}
