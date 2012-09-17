package org.tiramisu

import xml._
import dtd.{DTD}
import org.apache.commons.jexl2.{JexlContext, JexlEngine, Expression}
import java.io.{PrintWriter}
import collection.{Iterable, mutable}
import collection.JavaConversions._
import collection.convert.Wrappers

trait PageContext{
  val attributes = mutable.Map[String,AnyRef]()
  val route: List[PathItem]
}

case class PageCacheKey(pageName:String, template:Option[String])

sealed trait PageChunk{
  def write(context:PageContext, writer:PrintWriter)
}
case class StringChunk(content:String) extends PageChunk{
  def write(context:PageContext, writer:PrintWriter){
    writer.print(content)
  }
}

class PageCode{
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
    append(StringChunk(value.toString))
  }
}

trait Tag{
  def name:String

  implicit def nodePimp(node:Node)=new{
    def attributeAsText(key:String):Option[String]=node.attribute(key).map(_.text)
  }

  def run(elem:Elem, context:CompilationContext, pscope:NamespaceBinding=TopScope)
}

case class TagDescriptor(namespace:String, tags:Seq[Tag])

class CompilationContext{
  val attributes = mutable.Map[String,AnyRef]()
  var template:Option[String] = None
  val pageCode = new PageCode
}

trait Compositing { self:Tiramisu=>
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
    el.scope.buildString(sb, pscope)
    sb.append('>')
    print(sb)
  }

  private def writeElemEnd(el:Elem, context:CompilationContext, pscope:NamespaceBinding = TopScope){
    import context.pageCode._
    val sb = new StringBuilder
    sb.append("</")
    el.nameToString(sb)
    sb.append('>')
    print(sb)
  }

  private def writeElem(el:Elem, context:CompilationContext, pscope:NamespaceBinding = TopScope){
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
        descriptors(taggedElem.namespace).get(taggedElem.label) match {
          case Some(descriptor) =>  {
            descriptor.run(taggedElem, context)
          }
          case _ => writeElem(taggedElem, context, pscope) //sys.error("No tag "+taggedElem.label)
        }
      }
      case s: SpecialNode               => print(s buildString new StringBuilder)
      case g: Group                     => for (c <- g.nodes) processTags(c,context, g.scope)
      case _ => throw new IllegalArgumentException("Don't know how to serialize a " + node.getClass.getName)
    }
  }

  val loadedXmls = scala.collection.concurrent.TrieMap[String,Document]()

  def loadXml(name:String):Document=loadedXmls.getOrElseUpdate(name,{
   val xmlResourse = tiraviewPrefix+name+tiraviewSuffix;
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

  val tComposite = new Tag{
    def name = "composite"

    def run(elem: Elem, context: CompilationContext,pscope:NamespaceBinding=TopScope){
      for (template<-context.template){
        val xml = loadXml(template)
        if (!context.attributes.contains("dtd"))
          context.attributes.put("dtd",xml.dtd)
        context.attributes.put("currentPage",elem)
        processTags(xml.docElem, context, pscope)
      }
    }
  }

  val tContent = new Tag{
    def name = "content"

    def run(elem: Elem, context: CompilationContext, pscope:NamespaceBinding=TopScope){
      val page = context.attributes("currentPage").asInstanceOf[Elem]
      // TODO: bug here, what if no override specified
      val data = (for (dataItem <- page \\ "data"
                       if (dataItem.namespace == elem.namespace)
                         && (dataItem.attributeAsText("name")==elem.attributeAsText("name"))
      ) yield dataItem).head
      import context.pageCode._
      for (name<-elem.attributeAsText("name")){
        print("<t:content name='"+name+"'>")
      }
      data match {
        case someElem:Elem => for (el<-someElem.child) processTags(el,context,someElem.scope)
        case _ => for (el<-elem.child) processTags(el,context,elem.scope)
      }
      for (name<-elem.attributeAsText("name")){
        print("</t:content>")
      }
    }
  }

  class PageJexlContext(context:PageContext) extends JexlContext {
    def get(name: String): AnyRef = context.attributes(name)

    def set(name: String, value: AnyRef) {}

    def has(name: String): Boolean = context.attributes.contains(name)
  }

  val tOut = new Tag{
    def name = "out"
    val engine = new JexlEngine

    case class OutChunk(expression:Expression) extends PageChunk{
      def write(context: PageContext, writer:PrintWriter){
        writer.print(expression.evaluate(new PageJexlContext(context)))
      }
    }
    
    def run(elem: Elem, context: CompilationContext, pscope:NamespaceBinding=TopScope){
      val expressionText = elem.attributeAsText("value").getOrElse("");
      val expression = engine.createExpression(expressionText);
      context.pageCode.append(OutChunk(expression))
    }
  }

  val tFor = new Tag{
    def name = "for"
    val engine = new JexlEngine

    case class ForChunk(variable:String, expression:Expression, body:Iterable[PageChunk]) extends PageChunk{
      def write(context: PageContext, writer:PrintWriter){
        val oldValue = context.attributes.get(variable)
        val list = expression.evaluate(new PageJexlContext(context)).asInstanceOf[Wrappers.SeqWrapper[AnyRef]]
        for (i<-list){
          context.attributes.put(variable,i)
          for (bodyItem<-body){
            bodyItem.write(context, writer)
          }
        }
        oldValue match {
          case Some(old)=>context.attributes.put(variable,old)
          case None => context.attributes.remove(variable)
        }
      }
    }

    def run(elem: Elem, context: CompilationContext, pscope:NamespaceBinding=TopScope){
      val itemsText = elem.attributeAsText("items").getOrElse("");
      val varText = elem.attributeAsText("var").getOrElse("");
      val itemsExpression = engine.createExpression(itemsText);
      val compilationContext = new CompilationContext
      compilationContext.attributes ++= context.attributes
      for (el<-elem.child){
        processTags(el, compilationContext, elem.scope)
      }
      context.attributes.clear
      context.attributes ++= compilationContext.attributes
      context.pageCode.append(ForChunk(varText, itemsExpression, compilationContext.pageCode.code.reverse))
    }
  }

  val genericA = new Tag{
    def name = "a"

    def buildPath(href:String)={
      val pureHref = {
        val pos = href.indexOf('?')
        if (pos!= -1) href.substring(0,pos) else href
      }
      val stringPath =  pureHref.split('/')
      if (stringPath(0)=="")
        stringPath.toList.tail.map(StringPathItem(_))
      else
        routeConfiguration.get().route.reverse.tail.reverse:::
          stringPath.toList.filter(_!=".").map(StringPathItem(_))
    }
    
    def run(elem: Elem, context: CompilationContext, pscope: NamespaceBinding){

      val compilationContext = new CompilationContext
      compilationContext.attributes ++= context.attributes
      for (el<-elem.child){
        processTags(el, compilationContext, elem.scope)
      }
      context.attributes.clear
      context.attributes ++= compilationContext.attributes
      val body = compilationContext.pageCode.code.reverse
      val wc = WrappedChunk(elem, body, elem.scope, {(el, pageContext)=>
        val newClass = {
          elem.attributeAsText("class") match {
            case Some(value)=>value+" tiramisu-ajax-link";
            case None => "tiramisu-ajax-link"
          }
        }
        val md = new UnprefixedAttribute("class",newClass,Null)
        val thisTemplate = routeConfiguration.get().template
        val path = buildPath(elem.attributeAsText("href").get)
        val thatTemplate = routes.traverseDynamic(path).flatMap(_.configuration.template)
        val newElem = if (thisTemplate!=None && thisTemplate==thatTemplate)
          elem.copy(attributes=elem.attributes.append(md) )
        else
          elem
        newElem
      })

      context.pageCode.append(wc)
    }

  }

  val tResources = new Tag{
    def name = "resources"

    def run(elem: Elem, context: CompilationContext, pscope: NamespaceBinding) = {
      import context.pageCode._
      for (value <- elem.attributeAsText("value")){
        val resourceNames = value.split(",").filter(!_.isEmpty)
        for (resourceName<-resourceNames;
             resource<-resourcesMap.get(resourceName)){
            print(resource.resource(None)+"\n")
        }
      }
    }
  }

  case class WrappedChunk(elem:Elem,
                          body:Iterable[PageChunk],
                          pscope:NamespaceBinding,
                          transform:(Elem, PageContext)=>Elem) extends PageChunk{
    def write(context: PageContext, writer:PrintWriter){
      val newElem = transform(elem, context)
      val sb = new StringBuilder
      sb.append('<')
      newElem.nameToString(sb)
      if (newElem.attributes ne null) newElem.attributes.buildString(sb)
      newElem.scope.buildString(sb, pscope)
      sb.append('>')
      writer.print(sb)

      for (bodyItem<-body){
        bodyItem.write(context, writer)
      }

      sb.clear()
      sb.append("</")
      newElem.nameToString(sb)
      sb.append('>')
      writer.print(sb)
    }
  }

  val descriptors = toDescriptorMap(
    TagDescriptor("http://tiramisu.org/dev-0", List(tComposite,tContent,tOut,tFor,tResources)),
    TagDescriptor(null, List(genericA))
  )

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
    val compilationContext = new CompilationContext
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
    val page = loadPage(key)
    page.write(pageContext)
  }

  def compose(pageName:String, params:AnyRef*){
    compose(PageCacheKey(pageName, routeConfiguration.get.template), params:_*)
  }
}
