package com.gashu.myfpinscala.chapter8

import com.gashu.myfpinscala.chapter8.Prop.{FailedCase, SuccessCount}

/**
 * @author tiagogashu in 15/01/2020
 **/
sealed trait Prop[A] {

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop[A]): Prop[A]

}

class PropImpl[A] extends Prop[A] {

  override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
    this match {

      case Predicate(a, p) => if(p.apply(a)) Right(1) else Left(("Predicate failed"), 0)

      case And(p1, p2) => p1.check match {
        case Right(sCount) => p2.check match {
          case Right(sCount2) => Right(sCount + sCount2)
          case Left((f2, sCount2)) => Left(f2, sCount + sCount2)
        }
        case _ => _
      }

    }

  override def &&(p: Prop[A]): Prop[A] =
    And(this, p)

}

case class Predicate[A](a: A, predicate: A => Boolean) extends PropImpl[A]
case class And[A](thisP: Prop[A], thatP: Prop[A]) extends PropImpl[A]

object Prop {

  type FailedCase = String
  type SuccessCount = Int

}