package org.tiramisu

import java.io.PrintWriter

trait PageChunk {
  def write(context: PageContext, writer: PrintWriter)
}

case class StringChunk(content: String) extends PageChunk {
  def write(context: PageContext, writer: PrintWriter) {
    writer.print(content)
  }
}

case class OutChunk(es:ExpressionService, expression: String) extends PageChunk {
  def write(context: PageContext, writer: PrintWriter) {
    writer.print(es.loadExpression(expression).evaluate(new PageJexlContext(context)))
  }
}

trait ComputedChunk extends PageChunk {
  def compute(context: PageContext, writer: PrintWriter): List[PageChunk]

  def write(context: PageContext, writer: PrintWriter) = {
    val result = compute(context, writer)
    for (item <- result) {
      item.write(context, writer)
    }
  }
}

