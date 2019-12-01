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
      case Empty => Empty
      case Option(v) => Option(f(v))
    }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this match {
      case Empty => Empty
      case Option(v) => f(v)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Empty => default
      case Option(v) => v
    }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    this match {
      case Empty => ob
      case Option(v) => Option(v)
    }

  def filter(f: A => Boolean): MyOption[A] =
    this match {
      case Empty => Empty
      case Option(v) => if(f(v)) Option(v) else Empty
    }

}

case class Option[A](value: A) extends MyOptionImpl[A](value: A)
case object Empty extends MyOptionImpl
