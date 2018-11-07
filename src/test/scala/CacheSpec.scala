import FutureBool.all

import org.mockito.Mockito
import org.mockito.Mockito.{spy, verify}
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.{TableDrivenPropertyChecks, Tables}
import org.slf4j.{Logger, LoggerFactory}

import Predicate._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class CacheSpec extends FunSuite with ScalaFutures {
  val fakeLogger:Logger = Mockito.spy(classOf[Logger])
  val realLogger:Logger = LoggerFactory.getLogger("testLogger")
  val engine = new RulesEngine(realLogger)

  test("Rules can be constructed for fizzbuzz as a table") {
    val scenarios = Tables.Table[Int, String](
      ("param", "fizz or buzz or fizzbuzz or param"),
      (1, "1"),
      (2, "2"),
      (3, "fizz"),
      (4, "4"),
      (5, "buzz"),
      (6, "fizz"),
      (7, "7"),
      (8, "8"),
      (9, "fizz"),
      (10, "buzz"),
      (11, "11"),
      (12, "fizz"),
      (13, "13"),
      (14, "14"),
      (15, "fizzbuzz"),
      (16, "16")
    )

    val fizzBuzz = new FizzBuzz()

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.divisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.divisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString
    )

    TableDrivenPropertyChecks.forAll(scenarios) { (n, expected) =>
      val result = Await.result(engine.assessLogged(rules(n), "fizzbuzz"), 5 seconds).get
      assert(expected == result)
    }
  }

  test("fizzbuzz rules assessed for 15 call divisibleByThree once and divisibleByFive once") {
    val fizzBuzz = spy(new FizzBuzz())

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.divisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.divisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString
    )

    val result = Await.result(engine.assessLogged(rules(15), "fizzbuzz"), 5 seconds).get
    assert(result == "fizzbuzz")
    verify(fizzBuzz).divisibleByThree(15)
    verify(fizzBuzz).divisibleByFive(15)
  }

  test("fizzbuzz rules assessed for 1 call divisibleByThree once and divisibleByFive once to test caching") {
    val fizzBuzz = spy(new FizzBuzz())

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.mDivisibleByThree(n), fizzBuzz.mDivisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.mDivisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.mDivisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString
    )

    val result = Await.result(engine.assessLogged(rules(1), "memoized fizzbuzz"), 5 seconds).get
    assert(result == "1")
    verify(fizzBuzz).divisibleByThree(1)
    verify(fizzBuzz).divisibleByFive(1)
  }

  test("caching works with multiple params") {
    val fizzBuzz = spy(new FizzBuzz())

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.mDivisibleByThreeP(n), fizzBuzz.mDivisibleByFiveP(n))) -> "fizzbuzz",
      (() => fizzBuzz.mDivisibleByThreeP(n)) -> "fizz",
      (() => fizzBuzz.mDivisibleByFiveP(n)) -> "buzz",
      (() => Future(true)) -> n.toString
    )

    val result = Await.result(engine.assessLogged(rules(1), "memoized fizzbuzz"), 30 seconds).get
    assert(result == "1")
    verify(fizzBuzz).divisibleBy(1, 3)
    verify(fizzBuzz).divisibleBy(1, 5)
  }

  test("caching works with less time") {

    val fizzBuzz = spy(new FizzBuzz())

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.mDivisibleByThreeP(n), fizzBuzz.mDivisibleByFiveP(n))) -> "fizzbuzz",
      (() => fizzBuzz.mDivisibleByThreeP(n)) -> "fizz",
      (() => fizzBuzz.mDivisibleByFiveP(n)) -> "buzz",
      (() => Future(true)) -> n.toString
    )

    def time[R](block: => R): (R, Double) = {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      (result, (end - start) / 1e9)
    }

    val (result, secs) = time(Await.result(engine.assessLogged(rules(1), "memoized fizzbuzz"), 30 seconds).get)
    assert(result == "1")
    assert(secs > 1)

    val (result2, secs2) = time(Await.result(engine.assessLogged(rules(1), "memoized fizzbuzz"), 30 seconds).get)
    assert(result2 == "1")
    assert(secs2 < 1)
  }

  test("caching re-evaluates with new values") {
    val fizzBuzz = spy(new FizzBuzz())

    def rules(n: Int) = Seq(
      "All" -> (() => all(fizzBuzz.mDivisibleByThreeP(n), fizzBuzz.mDivisibleByFiveP(n))) -> "fizzbuzz",
      "Three" -> (() => fizzBuzz.mDivisibleByThreeP(n)) -> "fizz",
      "Five" -> (() => fizzBuzz.mDivisibleByFiveP(n)) -> "buzz",
      "None" -> (() => Future(true)) -> n.toString
    )

    def time[R](block: => R): (R, Double) = {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      (result, (end - start) / 1e9)
    }

    val (result, secs) = time(Await.result(engine.assessNamedLogged(rules(1), "memoized fizzbuzz"), 30 seconds).get)
    assert(result == "1")
    assert(secs > 1)

    val (result2, secs2) = time(Await.result(engine.assessNamedLogged(rules(2), "memoized fizzbuzz"), 30 seconds).get)
    assert(result2 == "2")
    assert(secs2 > 1)

    val (result3, secs3) = time(Await.result(engine.assessNamedLogged(rules(1)), 30 seconds).get)
    assert(result3 == "1")
    assert(secs3 < 1)
  }

  test("fizzbuzz rules are logged with the rule name and value") {
    val fizzBuzz = spy(new FizzBuzz())
    val fizzPredicate:Predicate[Int] = Predicate[Int](fizzBuzz.divisibleByThree, "fizz-predicate")
    val fizzBuzzPredicate:Predicate[Int] = Predicate[Int](n => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n)), "fizzbuzz-predicate")
    val buzzPredicate:Predicate[Int]  = Predicate[Int](fizzBuzz.divisibleByFive, "buzz-predicate")
    val `else`:Predicate[Int] = Predicate[Int](_ => Future.successful(true), "finish")

    val rules = Seq(
      fizzBuzzPredicate -> "fizzbuzz",
      fizzPredicate -> "fizz",
      buzzPredicate -> "buzz",
      `else` -> ""
    )

    val result = Await.result(engine.loggingAssess(rules)(15), 5 seconds).get
    assert(result == "fizzbuzz")
    verify(fizzBuzz).divisibleByThree(15)
    verify(fizzBuzz).divisibleByFive(15)
    verify(fakeLogger).info("Evaluting predicate fizzbuzz-predicate")
    verify(fakeLogger).info("Predicate fizzbuzz-predicate has value true")
    val result2 = Await.result(engine.loggingAssess(rules)(1), 5 seconds).get
    assert(result2 == "1")
  }

  //  test("fizzbuzz rules are logged with the rule name and value") {
  //    val fizzBuzz = spy(new FizzBuzz())
  //
  //    val fizzPredicate = Predicate(n, n2 => fizzBuzz.mDivisibleBy(n, n2), "fizz-predicate")
  //
  //    val fizzBuzzPredicate = Predicate(n, n3 => all(fizzBuzz.mDivisibleBy(n, n2), fizzBuzz.mDivisibleBy(n, n3)), "fizzbuzz-predicate")
  //
  //    val buzzPredicate = Predicate(fizzBuzz.mDivisibleBy(n, n3), "buzz-predicate")
  //
  //    val `else` = Predicate(_ => Future.successful(true), "finish")
  //
  //    val rules = Seq(
  //      fizzBuzzPredicate -> "fizzbuzz",
  //      fizzPredicate -> "fizz",
  //      buzzPredicate -> "buzz",
  //      `else` -> "number"
  //    )
  //    val result = Await.result(engine.loggingassessLogged(rules)(15), 5 seconds).get
  //    assert(result == "fizzbuzz")
  //    verify(fizzBuzz).divisibleByThree(15)
  //    verify(fizzBuzz).divisibleByFive(15)
  //    verify(fakeLogger).info("Evaluting predicate fizzbuzz-predicate")
  //    verify(fakeLogger).info("Predicate fizzbuzz-predicate has value true")
  //  }
}