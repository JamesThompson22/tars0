import FutureBool.all

import scala.concurrent.Future

object Predicate {

  val fizzBuzz = new FizzBuzz()

  val fizzPredicate:Predicate[Int] = Predicate[Int](fizzBuzz.divisibleByThree, "fizz-predicate")

  val fizzBuzzPredicate:Predicate[Int] = Predicate[Int](n => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n)), "fizzbuzz-predicate")

  val buzzPredicate:Predicate[Int] = Predicate[Int](fizzBuzz.divisibleByFive, "buzz-predicate")

  val `else`:Predicate[Int] = Predicate[Int](_ => Future.successful(true), "finish")
}

case class Predicate[A](function: A => Future[Boolean], name: String)