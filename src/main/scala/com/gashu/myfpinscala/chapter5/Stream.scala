package com.gashu.myfpinscala.chapter5

import scala.annotation.tailrec

/**
 * @author tiagogashu in 27/12/2019
 **/
sealed trait Stream[+A] {

  // 5.1
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  // 5.2
  def take(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(_, t) => if(n > 0) t().drop(n - 1) else t()
    }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}