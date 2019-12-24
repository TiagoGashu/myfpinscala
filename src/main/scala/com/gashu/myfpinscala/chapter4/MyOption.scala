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
      case Some(v) => Some(f(v))
    }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this match {
      case None => None
      case Some(v) => f(v)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(v) => v
    }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    this match {
      case None => ob
      case Some(v) => Some(v)
    }

  def filter(f: A => Boolean): MyOption[A] =
    this match {
      case None => None
      case Some(v) => if(f(v)) Some(v) else None
    }

}

object MyOptionImpl {

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  // 4.3
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    (a, b) match {
      case (a, b) if a == None || b == None => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }


}

case class Some[A](value: A) extends MyOptionImpl[A](value: A)
case object None extends MyOptionImpl
