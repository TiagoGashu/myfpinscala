package com.gashu.myfpinscala.chapter4

/**
 * @author tiagogashu in 30/11/2019
 **/
sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]
  def flatMap[B](f: A => MyOption[B]): MyOption[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B]
  def filter(f: A => Boolean): MyOption[A]
}

class MyOptionImpl[A](value: A) extends MyOption[A] {

  def map[B](f: A => B): MyOption[B] =
    this match {
      case None => None
      case Option(v) => Option(f(v))
    }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this match {
      case None => None
      case Option(v) => f(v)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Option(v) => v
    }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    this match {
      case None => ob
      case Option(v) => Option(v)
    }

  def filter(f: A => Boolean): MyOption[A] =
    this match {
      case None => None
      case Option(v) => if(f(v)) Option(v) else None
    }

}

object MyOptionImpl {

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  // 4.3
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    (a, b) match {
      case (a, b) if a == None || b == None => None
      case (Option(a), Option(b)) => Option(f(a, b))
    }


}

case class Option[A](value: A) extends MyOptionImpl[A](value: A)
case object None extends MyOptionImpl
