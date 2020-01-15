package com.gashu.myfpinscala.chapter8

/**
 * @author tiagogashu in 15/01/2020
 **/
sealed trait Prop[A] {

  def check: Boolean
  def &&(p: Prop[A]): Prop[A]

}

class PropImpl[A] extends Prop[A] {

  override def check: Boolean =
    this match {
      case Predicate(a, p) => p.apply(a)
      case And(p1, p2) => p1.check && p2.check
    }

  override def &&(p: Prop[A]): Prop[A] =
    And(this, p)

}

case class Predicate[A](a: A, predicate: A => Boolean) extends PropImpl[A]
case class And[A](thisP: Prop[A], thatP: Prop[A]) extends PropImpl[A]

object Prop {

}