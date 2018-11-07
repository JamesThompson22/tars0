import org.slf4j.Logger

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps


class RulesEngine(log: Logger) {

  def assess[V](rules: Seq[(() => Future[Boolean], V)]): Future[Option[V]] = {
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      accum flatMap {
        case v@Some(_) => Future(v)
        case None => curr._1().map {
          if (_) Some(curr._2) else None
        }
      }
    }
  }

  def assessLogged[V](rules: Seq[(() => Future[Boolean], V)], ruleset: String = "test"): Future[Option[V]] = {
    log.info(s"assessing ruleset $ruleset")
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      accum flatMap {
        case v@Some(_) =>  Future(v)
        case None => curr._1().map {
          if (_) {
            log.info("OUTCOME: " + curr._2)
            Some(curr._2)
          } else {
            log.debug("rule not met: " + curr._2)
            None
          }
        }
      }
    }
  }

  def loggingAssess[S, V](rules: Seq[(Predicate[S], V)])(value: S): Future[Option[V]] = {
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      val (predicate, outcome) = curr
      accum flatMap {
        case v@Some(_) => Future(v)
        case None =>
          log.info(s"Evaluting predicate ${predicate.name}")
          predicate.function(value).map { predicateResult =>
            log.info(s"Predicate ${predicate.name} has value $predicateResult")
            if (predicateResult) {
              Some(outcome)
            } else {
              None
            }
          }
      }
    }
  }
}
