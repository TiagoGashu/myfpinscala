package com.gashu.myfpinscala.chapter8

import com.gashu.myfpinscala.chapter6.RNG
import com.gashu.myfpinscala.chapter8.Prop.{FailedCase, SuccessCount, TestCases}

/**
 * @author tiagogashu in 15/01/2020
 **/
sealed trait Prop {

  def check: Result
  def &&(p: Prop): Prop
  def ||(p: Prop): Prop

}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  case class Prop(run: (TestCases,RNG) => Result)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}