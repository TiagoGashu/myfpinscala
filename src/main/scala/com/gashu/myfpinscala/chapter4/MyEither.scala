package com.gashu.myfpinscala.chapter4

/**
 * @author tiagogashu in 25/12/2019
 **/
sealed trait MyEither[+E, +A] {

  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):Either[EE, C] = ???

}

case class Left[+E](error: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]