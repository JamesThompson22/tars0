import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class RulesEngineSpec extends FunSuite with ScalaFutures {
  val engine = new RulesEngine

  test("An empty sequence of rules returns a None as a consequence when asseses") {
    val result = Await.result(engine.assess(Seq()), 5 seconds)
    assert(result == None)
  }

  test("A sequence of rules returns the value of the leftmost rule where the precedent evaluates to Future.successful(true) when assessed") {
    val rules = Seq((() => Future(true), "FIRST-TRUE-RULE"), (() => Future(false), "FALSE-RULE"))
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with some precedents evaluating to false at the beginning, skips over them to the first true rule") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(true))  -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "SECOND-FALSE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with some precedents evaluating to false at the beginning, skips over them to the first true rule, subsequent true rules are ignored") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(true))  -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "SECOND-FALSE-RULE",
      (() => Future(true)) -> "SECOND-TRUE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with all precedents evaluating to false returns None") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(false))  -> "SECOND-FALSE-RULE",
      (() => Future(false)) -> "THIRD-FALSE-RULE",
      (() => Future(false)) -> "FOURTH-FALSE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == None)
  }

  test("A sequence of rules with an early true followed by a failure returns the value associated with the true") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(true))  -> "FIRST-TRUE-RULE",
      (() => Future.failed(new RuntimeException("fail"))) -> "FAIL",
      (() => Future(false)) -> "THIRD-FALSE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with an early fail followed by a true returns the failure") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future.failed(new RuntimeException("fail"))) -> "FAIL",
      (() => Future(true))  -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "THIRD-FALSE-RULE"
    )
    val result = intercept[RuntimeException] {
      Await.result(engine.assess(rules), 5 seconds)
    }
    assert(result.getMessage == "fail")
  }

  test("A sequence of rules with an early fail followed another fail followed by a true returns the first failure") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future.failed(new RuntimeException("fail0"))) -> "FAIL0",
      (() => Future.failed(new RuntimeException("fail1"))) -> "FAIL1",
      (() => Future(true))  -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "THIRD-FALSE-RULE"
    )
    val result = intercept[RuntimeException] {
      Await.result(engine.assess(rules), 5 seconds)
    }
    assert(result.getMessage == "fail0")
  }

  test("Finding an earlier true rule prevents execution of later precedents") {}
}
