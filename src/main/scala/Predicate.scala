import FutureBool.all

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Predicate {

}

case class Predicate[A](function: A => Future[Boolean], name: String)