package org.tiramisu

import org.apache.commons.jexl2.{Expression, JexlEngine, JexlContext}

class PageJexlContext(context: PageContext) extends JexlContext {
  def get(name: String): AnyRef = context.attributes(name)

  def set(name: String, value: AnyRef) {}

  def has(name: String): Boolean = context.attributes.contains(name)
}

trait ExpressionService {
  private[this] val engine = new JexlEngine

  private[this] val expressions = scala.collection.concurrent.TrieMap[String, Expression]()

  def loadExpression(expression: String) =
    expressions.getOrElseUpdate(expression, engine.createExpression(expression))
}

