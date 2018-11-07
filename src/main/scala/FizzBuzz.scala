import com.softwaremill.macmemo.memoize

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps

class FizzBuzz {
  def divisibleByThree(n: Int): Future[Boolean] = Future(n % 3 == 0)

  def divisibleByFive(n: Int): Future[Boolean] = Future(n % 5 == 0)

  @memoize(2000, 2 hours)
  def mDivisibleByThree(n: Int): Future[Boolean] = divisibleByThree(n)

  @memoize(maxSize = 2000, expiresAfter = 2 hours)
  def mDivisibleByFive(n: Int): Future[Boolean] = divisibleByFive(n)

  def divisibleBy(n: Int, d: Int): Future[Boolean] = {
    Thread.sleep(1000); Future(n % d == 0)
  }

  @memoize(2000, 100 second)
  def mDivisibleByThreeP(n: Int): Future[Boolean] = divisibleBy(n, 3)

  @memoize(maxSize = 2000, expiresAfter = 100 second)
  def mDivisibleByFiveP(n: Int): Future[Boolean] = divisibleBy(n, 5)

  @memoize(2000, 100 seconds)
  def mDivisibleBy(n: Int, n2: Int):Future[Boolean] = divisibleBy(n, n2)
}
