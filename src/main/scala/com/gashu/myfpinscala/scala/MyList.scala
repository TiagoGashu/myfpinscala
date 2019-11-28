package com.gashu.myfpinscala.scala

import scala.annotation.tailrec

/**
 * @author tiagogashu in 27/11/2019
 **/

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def foldRight[A, B](as: MyList[A], baseCase: B)(f: (A, B) => B): B =
    as match {
      case Nil => baseCase
      case Cons(x, xs) => f(x, foldRight(xs, baseCase)(f))
    }

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  // 3.2
  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Nil => throw new UnsupportedOperationException("List is empty!")
    case Cons(_, tail) => tail
  }

  // 3.3
  def setHead[A](l: MyList[A], newHead: A): MyList[A] = l match {
    case Nil => Cons(newHead, Nil)
    case Cons(_, tail) => Cons(newHead, tail)
  }

  // 3.4
  @tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] = (l, n) match {
    case (Nil, n) if n > 0 => throw new UnsupportedOperationException("No elements to drop!")
    case (list, 0) => list
    case (Cons(_, tail), n) => drop(tail, n - 1)
  }

  // 3.5
  @tailrec
  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
    case Nil => Nil
    case Cons(head: A, tail) if f(head) => dropWhile(tail)(f)
    case _ => l
  }

  def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = l1 match {
    case Nil => l2
    case Cons(h, t) => Cons(h, append(t, l2))
  }

  // 3.6 O(n)
  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => Nil
    case Cons(h, Cons(_, Nil)) => Cons(h, Nil)
    case Cons(h, t) => Cons(h, init(t))
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}