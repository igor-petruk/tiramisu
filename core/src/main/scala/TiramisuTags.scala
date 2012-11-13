package org.tiramisu

import xml._
import collection.convert.Wrappers
import collection.{Iterable}
import java.io.PrintWriter
import collection.JavaConversions._
import xml.NamespaceBinding
import scala.Some

case class ForChunk(es:ExpressionService, variable:String, expression:String, body:Iterable[PageChunk]) extends PageChunk{
  def write(context: PageContext, writer:PrintWriter){
    val oldMap = context.attributes
    val list = es.loadExpression(expression).evaluate(
      new PageJexlContext(context)).asInstanceOf[Wrappers.SeqWrapper[AnyRef]]
    var item: AnyRef = null
    def getItemMap = Map(variable->{()=>item})
    context.attributes = getItemMap.orElse(context.attributes)
    for (i<-list){
      item = i
      for (bodyItem<-body){
        bodyItem.write(context, writer)
      }
    }
    context.attributes = oldMap
  }
}

trait TiramisuTags {self: Compositing with Resources =>
  val tiramisuTags = TagDescriptor("http://tiramisu.org/dev-0",
    List(new TOut,
      new TFor,
      new TResources,
      new TUse,
      new TInsert
    )
    )

    class TInsert extends Tag("insert"){

      def run(elem: Elem, context: CompilationContext, pscope: NamespaceBinding) {
        this.logger.debug("INSERT {}/{}",Array(elem,pscope):_*)
        val data = elem.attributeAsText("name").map(context.dataScope(_))
        for (dataElem<-data){
          processTags(dataElem, context, pscope)
        }
      }
    }

    class TUse extends Tag("use"){
    def run(elem: Elem, context: CompilationContext, pscope: NamespaceBinding) {
      this.logger.debug("USE {}/{}",Array(elem,pscope):_*)
      val oldData = context.dataScope
      val data = for (item <- elem \\ "data" if (item.namespace == elem.namespace)) yield item
      context.dataScope ++= data.map{item=>(item.attributeAsText("name").get,Group(item.child))}.toMap

      val xml = loadXml(elem.attributeAsText("component").get)
      if (!context.attributes.contains("dtd"))
        context.attributes.put("dtd",xml.dtd)
      context.attributes.put("currentPage",elem)
      for(node<-xml.docElem.child)
      processTags(node, context,xml.docElem.scope)

      context.dataScope = oldData
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
      this.logger.debug("FOR {}/{}",Array(elem,pscope):_*)
      val itemsText = elem.attributeAsText("items").getOrElse("");
      val varText = elem.attributeAsText("var").getOrElse("");
      val body = doBody(elem, context, pscope)
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