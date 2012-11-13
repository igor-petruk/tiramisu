package org.tiramisu

trait BeanProvider{
  def provide(id:String):Option[AnyRef]

  def store(id:String, item:Option[AnyRef])
}

class Bean[T](provider:BeanProvider, factory: Option[()=>T]){
  var beanId:String = _

  def value:T = {
    val value = provider.provide(beanId).orNull
    if (value==null){
      factory match {
        case Some(providedFactory) => {
          synchronized{
            val secondCheckValue = provider.provide(beanId).orNull
            if (secondCheckValue==null){
              val newValue = providedFactory()
              value_=(newValue)
              newValue
            }else{
              secondCheckValue.asInstanceOf[T]
            }
          }
        }
        case None => throw new NoSuchElementException
      }
    }else{
      value.asInstanceOf[T]
    }
  }

  def value_=(that:T){
     println("Storing "+beanId+" "+that)
     provider.store(beanId, Option(that.asInstanceOf[AnyRef]))
  }

  def delete{
    provider.store(beanId,None)
  }

  def option = provider.provide(beanId)
}

class ScopedBeanShortcut(provider:BeanProvider){
  def apply[T<:Any](factory: =>T) = new Bean[T](provider, Some(()=>factory))
  def apply[T<:AnyRef]() = new Bean[T](provider, None)
}

trait SessionBeans {self:Controller=>
  private[this] class SessionBeanProvider(controller:Controller) extends BeanProvider{
    def provide(id: String) = {
      Some(controller.request.getSession.getAttribute(id))
    }

    def store(id: String, item: Option[AnyRef]) = {
      item match {
        case Some(value) => controller.request.getSession.setAttribute(id,value)
        case None => controller.request.getSession.removeAttribute(id)
      }
    }
  }

  private[this] val sessionBeanProvider = new SessionBeanProvider(this)

  def sessionBean = new ScopedBeanShortcut(sessionBeanProvider)
}

trait RequestBeans {self:Controller=>
  private[this] class RequestBeanProvider(controller:Controller) extends BeanProvider{
    def provide(id: String) = {
      Option(controller.request.getAttribute(id))
    }

    def store(id: String, item: Option[AnyRef]) = {
      item match {
        case Some(value) => controller.request.setAttribute(id,value)
        case None => controller.request.removeAttribute(id)
      }
    }
  }

  private[this] val requestBeanProvider = new RequestBeanProvider(this)

  def requestBean = new ScopedBeanShortcut(requestBeanProvider)
}
