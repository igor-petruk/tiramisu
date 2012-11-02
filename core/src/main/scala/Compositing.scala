package org.tiramisu

import xml._
import dtd.{DTD}
import org.apache.commons.jexl2.{JexlContext, JexlEngine, Expression}
import java.io.{PrintWriter}
import collection.{immutable, Iterable, mutable}
import collection.JavaConversions._
import collection.convert.Wrappers
import javax.servlet.Servlet
import annotation.tailrec
import org.slf4j.LoggerFactory

class CompilationContext(es:ExpressionService){
  val attributes = mutable.Map[String,AnyRef]()
  var template:Option[String] = None
  val pageCode = new PageCode(es)
	var dataScope = immutable.Map[String,Node]()
  def expressionService = es
}

trait PageContext{
  val attributes = mutable.Map[String,AnyRef]()
  val route: List[PathItem]
}

case class PageCacheKey(pageName:String, template:Option[String])

case class TagDescriptor(namespace:String, tags:Seq[Tag])

abstract class Tag(fName:String){
  protected val logger = LoggerFactory.getLogger(this.getClass)

  def name:String = fName

  implicit def nodePimp(node:Node)=new{
    def attributeAsText(key:String):Option[String]=node.attribute(key).map(_.text)
  }

  def run(elem:Elem, context:CompilationContext, pscope:NamespaceBinding=TopScope)
}

class PageCode(es:ExpressionService){
  var code: List[PageChunk] = Nil

  def append(chunk: PageChunk){
    code = code match {
      case (StringChunk(headContent))::tail => chunk match {
        case StringChunk(content) => StringChunk(headContent+content)::tail
        case _ => chunk::code
      }
      case _ => chunk::code
    }
  }

  def print(value:Any){
    pushString(value.toString)

    @tailrec
    def pushString(str:String){
      val start = str.indexOf("${");
      if (start!= -1){
        append(StringChunk(str.substring(0,start)))
        val end = str.indexOf("}",start)
        val expressionString = str.substring(start+2,end)
        append(OutChunk(es,expressionString))
        pushString(str.substring(end+1))
      }else{
        append(StringChunk(str))
      }
    }
  }
}

// test

trait Compositing extends TiramisuTags
				          with GeneralTags
                  with ExpressionService { self:Tiramisu=>
  private[Compositing] val logger = LoggerFactory.getLogger(classOf[Compositing])

  def tiraviewPrefix = "/WEB-INF/tiraview/"
  def tiraviewSuffix = ".xhtml"

  implicit def stringPimp(s:String) = new {
    def withTemplate(template:String) = PageCacheKey(s,Some(template))
  }

  var syntacticScopeConfiguration = RouteConfiguration()

  private def writeElemStart(el:Elem, context:CompilationContext, pscope:NamespaceBinding = TopScope){
    import context.pageCode._
    val sb = new StringBuilder
    sb.append('<')
    el.nameToString(sb)
    if (el.attributes ne null) el.attributes.buildString(sb)
    if (el.scope!=pscope)
    el.scope.buildString(sb, pscope)
    sb.append('>')
    logger.debug("Written {} start scope({}) pscope=({}) {}",el,el.scope, pscope,sb)
    print(sb)
  }

  private def writeElemEnd(el:Elem, context:CompilationContext, pscope:NamespaceBinding = TopScope){
    import context.pageCode._
    val sb = new StringBuilder
    sb.append("</")
    el.nameToString(sb)
    sb.append('>')
    print(sb)
    logger.debug("Written {} end pscope=({}) {}",Array(el,pscope,sb):_*)
  }

  def writeElem(el:Elem, context:CompilationContext, pscope:NamespaceBinding = TopScope){
    writeElemStart(el, context, pscope)
    for (child<-el.child){
      processTags(child,context,el.scope)
    }
    writeElemEnd(el, context, pscope)
  }
  
  def processTags(node:Node, context:CompilationContext, pscope:NamespaceBinding = TopScope){
    import context.pageCode._
    node match {
      case taggedElem:Elem =>{
        val tags = descriptors.get(taggedElem.namespace)
        tags match {
          case Some(tagMap) => descriptors(taggedElem.namespace).get(taggedElem.label) match {
              case Some(descriptor) =>  {
                descriptor.run(taggedElem, context,pscope)
              }
              case _ => writeElem(taggedElem, context, pscope) //sys.error("No tag "+taggedElem.label)
            }
          case _ => writeElem(taggedElem, context, pscope) //sys.error("No tag "+taggedElem.label)
        }
      }
      case s: SpecialNode               => print(s buildString new StringBuilder)
      case g: Group                     => {
        logger.debug("GROUP {}/{}",Array(g,g.scope):_*)
        for (c <- g.nodes) processTags(c,context, pscope)
      }
      case _ => throw new IllegalArgumentException("Don't know how to serialize a " + node.getClass.getName)
    }
  }

  val loadedXmls = scala.collection.concurrent.TrieMap[String,Document]()

  def loadXml(name:String):Document=loadedXmls.getOrElseUpdate(name,{
   val xmlResourse = tiraviewPrefix+name+tiraviewSuffix;
   logger.debug("Loading: {}",xmlResourse);
    val parser = scala.xml.parsing.ConstructingParser.fromSource(io.Source.fromInputStream(filterConfig.getServletContext().getResourceAsStream(xmlResourse)), true)
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

  def doBody(elem:Elem, context:CompilationContext, pscope:NamespaceBinding) = {
    val compilationContext = new CompilationContext(this)
    compilationContext.attributes ++= context.attributes
    for (el<-elem.child){
      processTags(el, compilationContext, pscope)
    }
    context.attributes.clear
    context.attributes ++= compilationContext.attributes
    compilationContext.pageCode.code.reverse
  }

  implicit def func2cc(f:(PageContext, PrintWriter)=>List[PageChunk]) = new ComputedChunk {
    def compute(context: PageContext, writer: PrintWriter) = f(context,writer)
  }

  val descriptors = toDescriptorMap(tiramisuTags, generalTags)

  class Page(fCode:List[PageChunk]){
    var dtd:DTD = null

    val code = fCode

    def write(pageContext:PageContext){
      if (dtd!=null){
        out.println(
         "<!DOCTYPE html %s%s>".format(
           Option(dtd.externalID) getOrElse "",
           dtd.decls.mkString("", "\n", "")
         )
       )
      }
      for (chunk<-code){
        chunk.write(pageContext, out)
      }
    }
  }

  val loadedPages = scala.collection.concurrent.TrieMap[PageCacheKey,Page]()

  def loadPage(key:PageCacheKey):Page=loadedPages.getOrElseUpdate(key,{
    val pageXml = loadXml(key.pageName)
    val compilationContext = new CompilationContext(this)
    compilationContext.template = key.template
    processTags(pageXml.docElem, compilationContext)
    val page = new Page(compilationContext.pageCode.code.reverse)
    for (dtdRef <- compilationContext.attributes.get("dtd")){
      page.dtd = dtdRef.asInstanceOf[DTD]
    }
    page
  })

  def convertInput(data:AnyRef)={
    data match {
      case seq:Seq[AnyRef] => seq:java.util.List[AnyRef]
      case map:Map[AnyRef,AnyRef] => map:java.util.Map[AnyRef,AnyRef]
      case other => other
    }
  }

  def template(name:String)(body: =>Unit){
    val old = syntacticScopeConfiguration
    syntacticScopeConfiguration = old.copy(template=Some(name))
    body;
    syntacticScopeConfiguration = old;
  }

  def compose(key:PageCacheKey, params:AnyRef*){
    val effectiveKey = if (noTemplateRequest) key.copy(template = None) else key
    val map = (for (value<-params) yield
      value match {
        case (key:String, data:AnyRef)=> (key->convertInput(data))
        case other=> (value.getClass.getSimpleName->convertInput(value))
      }
      ).toMap[String,AnyRef]
    response.setContentType("text/html")//; charset=utf-8")
    response.setCharacterEncoding("utf-8")
    val pageContext = new PageContext{
      val route = routeConfiguration.get().route
    }
    pageContext.attributes ++= map
    val page = loadPage(effectiveKey)
    page.write(pageContext)
  }

  def compose(pageName:String, params:AnyRef*){
    compose(PageCacheKey(pageName, routeConfiguration.get.template), params:_*)
  }
}
