package org.tiramisu

import xml.{NamespaceBinding, Null, UnprefixedAttribute, Elem}
import java.io.PrintWriter

trait GeneralTags {
  self: Tiramisu =>

  val generalTags = TagDescriptor(null, List())

}
