package org.tiramisu

import xml.{NamespaceBinding, Null, UnprefixedAttribute, Elem}
import java.io.PrintWriter

trait GeneralTags {
  self: Tiramisu =>

  val generalTags = TagDescriptor(null, List(new GenericA))

  class GenericA extends Tag("a") {
    def buildPath(href: String) = {
      val pureHref = {
        val pos = href.indexOf('?')
        if (pos != -1) href.substring(0, pos) else href
      }
      val stringPath = pureHref.split('/')
      if (stringPath(0) == "")
        stringPath.toList.tail.map(StringPathItem(_))
      else
        routeConfiguration.get().route.reverse.tail.reverse :::
          stringPath.toList.filter(_ != ".").map(StringPathItem(_))
    }

    def transform(elem: Elem, pageContext: PageContext) = {
      val newClass = {
        elem.attributeAsText("class") match {
          case Some(value) => value + " tiramisu-ajax-link";
          case None => "tiramisu-ajax-link"
        }
      }
      val md = new UnprefixedAttribute("class", newClass, Null)
      val thisTemplate = routeConfiguration.get().template
      val path = buildPath(elem.attributeAsText("href").get)
      val thatTemplate = routes.traverseDynamic(path).flatMap(_.configuration.template)
      val newElem = if (thisTemplate != None && thisTemplate == thatTemplate)
        elem.copy(attributes = elem.attributes.append(md))
      else
        elem
      newElem
    }

    def run(elem: Elem, context: CompilationContext, pscope: NamespaceBinding) {
      def processA(pageContext: PageContext, writer: PrintWriter): List[PageChunk] = {
        val newElem = transform(elem, pageContext)
        val cc = new CompilationContext(self)
        cc.attributes ++= context.attributes
        writeElem(newElem, cc, pscope)
        cc.pageCode.code.reverse
      }

      context.pageCode.append(processA _)
    }

  }

}
