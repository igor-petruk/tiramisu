package org.tiramisu

import collection.mutable.{ArrayBuffer, ListBuffer, SynchronizedBuffer}

trait Exposure{self:Controller=>
  var exposureList = new ArrayBuffer[Bean[AnyRef]] with SynchronizedBuffer[Bean[AnyRef]]
  var exposureMap:Map[String, ()=>AnyRef] = Map()

  implicit def exposurePimp[T<:AnyRef](bean:Bean[T]) = new {
    def expose={
      exposureList+=(bean.asInstanceOf[Bean[AnyRef]])
      bean
    }
  }

  postLoad{
    val exposureBeanMap = exposureList.groupBy(_.beanId).mapValues(_.head)
    exposureMap = exposureBeanMap.mapValues({bean=>{()=>bean.value}})
    exposureList.clear
  }
}