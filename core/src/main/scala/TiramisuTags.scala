package org.tiramisu

import xml.{TopScope, NamespaceBinding, Elem}
import collection.convert.Wrappers
import collection.{Iterable}
import java.io.PrintWriter
import collection.JavaConversions._

case class ForChunk(es:ExpressionService, variable:String, expression:String, body:Iterable[PageChunk]) extends PageChunk{
  def write(context: PageContext, writer:PrintWriter){
    val oldValue = context.attributes.get(variable)
    val list = es.loadExpression(expression).evaluate(
      new PageJexlContext(context)).asInstanceOf[Wrappers.SeqWrapper[AnyRef]]
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

trait TiramisuTags {self: Compositing with Resources =>
  val tiramisuTags = TagDescriptor("http://tiramisu.org/dev-0",
    List(new TComposite,
      new TContent,
      new TOut,
      new TFor,
      new TResources)
    )

  class TComposite extends Tag("composite"){

    def run(elem: Elem, context: CompilationContext,pscope:NamespaceBinding=TopScope){
      for (template<-context.template){
        val xml = loadXml(template)
        if (!context.attributes.contains("dtd"))
          context.attributes.put("dtd",xml.dtd)
        context.attributes.put("currentPage",elem)
        processTags(xml.docElem, context, pscope)
      }
      if (context.template==None){
        writeElem(elem, context, pscope)
      }
    }
  }

  class TContent extends Tag("content"){
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

  class TOut extends Tag("out"){
    def run(elem: Elem, context: CompilationContext, pscope:NamespaceBinding=TopScope){
      val expressionText = elem.attributeAsText("value").getOrElse("");
      context.pageCode.append(OutChunk(context.expressionService,expressionText))
    }
  }

  class TFor extends Tag("for"){
    def run(elem: Elem, context: CompilationContext, pscope:NamespaceBinding=TopScope){
      val itemsText = elem.attributeAsText("items").getOrElse("");
      val varText = elem.attributeAsText("var").getOrElse("");
      val body = doBody(elem, context)
      context.pageCode.append(ForChunk(context.expressionService,varText, itemsText, body))
    }
  }

  class TResources extends Tag("resources"){
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

}