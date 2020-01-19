package com.gashu.myfpinscala.chapter8

import com.gashu.myfpinscala.chapter6.{RNG,SimpleRNG}
import com.gashu.myfpinscala.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

/**
 * @author tiagogashu in 15/01/2020
 **/
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

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {

  def &&(p: Prop): Prop =
    Prop {
      (max, n, rng) => run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case f@Falsified(_, _) => f
      }
    }

  def ||(p: Prop): Prop =
    Prop {
      (max, n, rng) => run(max, n, rng) match {
        case passed@Passed => passed
        case Falsified(failureMsg, _) => p.tag(failureMsg).run(max, n, rng)
      }
    }

  def tag(msg: String): Prop =
    Prop {
      (max, n, rng) => run(max, n, rng) match {
        case Falsified(failureMsg, successCount) => Falsified(msg + ":" + failureMsg, successCount)
        case x => x
      }
    }

}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
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

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g.forSize(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
    }
  }

  def run(p: Prop,
  maxSize: Int = 100,
  testCases: Int = 100,
  rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }

}
