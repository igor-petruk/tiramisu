package org.tiramisu

import collection.mutable.{ArrayBuffer, ListBuffer, SynchronizedBuffer}

trait Exposure{self:Controller=>
  var exposureList = new ArrayBuffer[Bean[Any]] with SynchronizedBuffer[Bean[Any]]
  var exposureMap:Map[String, ()=>Any] = Map()

  implicit def exposurePimp[T](bean:Bean[T]) = new {
    def expose={
      exposureList+=(bean.asInstanceOf[Bean[Any]])
      bean
    }
  }

  postLoad{
    val exposureBeanMap = exposureList.groupBy(_.beanId).mapValues(_.head)
    exposureMap = exposureBeanMap.mapValues({bean=>{()=>bean.value}})
    exposureList.clear
  }
}