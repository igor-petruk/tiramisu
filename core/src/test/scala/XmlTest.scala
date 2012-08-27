package org.tiramisu

import org.junit.Test
import xml.NodeSeq._
import xml.{TopScope, NodeSeq, Elem, Node}

trait Tag{
  def name:String
  
  implicit def nodePimp(node:Node)=new{
    def attributeAsText(key:String):Option[String]=node.attribute(key).map(_.text)
  }

  def transform(elem:Elem):Option[NodeSeq]
}

case class TagDescriptor(namespace:String, tags:Seq[Tag])

class XmlTest {
  

  val template1 =
    <html xmlns:t="http://tiramisu.org/dev-0">
      <head>
        <title>Home page</title>
        <t:content name="head"/>
      </head>
      <body>
        <div id="content">
          <t:content name="body">
            Simple content
          </t:content>
        </div>
      </body>
    </html>

  val templates = Map("template1"->template1)

  val page1 =
    <t:composite template="template1" xmlns:t="http://tiramisu.org/dev-0">
      <t:data name="head">
        <script src="my.js"/>
      </t:data>
      <t:data name="body">
        <h2> We won!!! </h2>
        Here goes text
      </t:data>
    </t:composite>

   @Test
   def test1(){
     println("Page:")
      println(page1)
      println()

     println("Template:")
     println(template1)
     println()

     println("Result:")
    // println(processTags(page1))


   }

}
