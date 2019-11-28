package com.gashu.myfpinscala.scala

import scala.annotation.tailrec

/**
 * @author tiagogashu in 27/11/2019
 **/

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ints: MyList[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(ds: MyList[Double]): Double = foldRight(ds, 0.0)(_ * _)

  // 3.2
  def tail[A](l: MyList[A]): MyList[A] = l match {
    case Nil => throw new RuntimeException("List is empty!")
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
    case (Nil, n) if n > 0 => throw new RuntimeException("No elements to drop!")
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
    case Nil => throw new RuntimeException("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // 3.9 length using foldRight
  // the right el is the result of the rec call to foldRight
  def length[A](l: MyList[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  // 3.10 fold left
  @tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, tail) => foldLeft(tail, f(x, z))(f)
    }

  def sumFoldLeft(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productFoldLeft(ds: MyList[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def lengthFoldLeft[A](l: MyList[A]): Int = foldLeft(l, 0)((_, acc) => 1 + acc)

  // 3.11 reverse
  def reverseFoldLeft[A](l: MyList[A]): MyList[A] =
    foldLeft[A, MyList[A]](l, Nil)((el, acc) => append(Cons(el, Nil), acc))

  def reverseFoldRight[A](l: MyList[A]): MyList[A] =
    foldRight[A, MyList[A]](l, Nil)((el, acc) => append(acc, Cons(el, Nil)))

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}