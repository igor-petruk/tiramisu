package org.tiramisu

import xml._
import dtd.{DTD}
import org.apache.commons.jexl2.{JexlContext, JexlEngine, Expression}
import java.io.{PrintWriter}
import collection.{mutable, immutable, Iterable}
import collection.JavaConversions._
import scala.concurrent._
import annotation.tailrec
import org.slf4j.LoggerFactory
import scala.concurrent.duration._

class CompilationContext(es:ExpressionService){
  val attributes = mutable.Map[String,AnyRef]()
  val pageCode = new PageCode(es)
	var dataScope = immutable.Map[String,Node]()
  def expressionService = es
}

trait PageContext{
  var attributes:PartialFunction[String,()=>AnyRef] = Map()
  val route: List[PathItem]
}

case class PageCacheKey(pageName:String)

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
                  with ExpressionService
                  with Concurrency
                  { self:Tiramisu=>
  private[Compositing] val logger = LoggerFactory.getLogger(classOf[Compositing])

  private implicit val ec = executionContext

  def tiraviewPrefix = "/WEB-INF/tiraview/"
  def tiraviewSuffix = ".xhtml"

  implicit def stringPimp(s:String) = new {

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

  val removePreceedingSpace = true

  def packCode(code:List[PageChunk])={
    println(code)
    if (removePreceedingSpace){
       val charsToSkip = Set(' ','\n','\r','\t')
       code match {
         case StringChunk(beginning)::tail=>StringChunk(beginning.dropWhile(charsToSkip.contains(_)))::tail
         case x=>x
       }
     }else{
       code
     }
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

  val loadedPages =  new mutable.HashMap[PageCacheKey,Future[Page]] with mutable.SynchronizedMap[PageCacheKey,Future[Page]]

  def loadPage(key:PageCacheKey):Page=Await.result(loadedPages.getOrElseUpdate(key,future({
    val pageXml = loadXml(key.pageName)
    val compilationContext = new CompilationContext(this)
    processTags(pageXml.docElem, compilationContext)
    val pageCode = packCode(compilationContext.pageCode.code.reverse)
    val page = new Page(pageCode)
    compilationContext.attributes.get("dtd") match {
      case Some(dtdRef)=>page.dtd = dtdRef.asInstanceOf[DTD]
      case None => page.dtd = pageXml.dtd
    }
    for (dtd<-Option(page.dtd)){
      logger.debug("Found DTD {}",dtd)
    }
    page
  })),Duration.Inf)

  def convertInput(data:AnyRef):AnyRef={
    data match {
      case seq:Seq[AnyRef] => seq:java.util.List[AnyRef]
      case map:Map[AnyRef,AnyRef] => map:java.util.Map[AnyRef,AnyRef]
      case opt:Option[AnyRef] => opt.toList:java.util.List[AnyRef]
      case bean:Bean[AnyRef] => convertInput(bean.value)
      case other => {
        other
      }
    }
  }

  def compose(key:PageCacheKey, params:(String,AnyRef)*){
    val effectiveKey = key
    val map = params.toMap[String,AnyRef].mapValues(x=>convertInput(x)).mapValues({x=>{()=>x}})

    response.setContentType("text/html")//; charset=utf-8")
    response.setCharacterEncoding("utf-8")
    val pageContext = new PageContext{
      val route = routeConfiguration.get().route
    }
    pageContext.attributes = map.orElse(exposureMap.mapValues({f=>{()=>convertInput(f())}}))
    val page = loadPage(effectiveKey)
    page.write(pageContext)
  }

  def compose(pageName:String, params:(String,AnyRef)*){
    compose(PageCacheKey(pageName), params:_*)
  }
}
