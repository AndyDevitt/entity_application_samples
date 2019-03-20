package sample1.domain.cta

sealed trait ContextFilter

object ContextFilter {

  case class SimpleContextFilter(contextPath: String) extends ContextFilter

}

