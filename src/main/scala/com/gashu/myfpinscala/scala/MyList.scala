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

  // 3.11
  def sumFoldLeft(ints: MyList[Int]): Int = foldLeft(ints, 0)(_ + _)
  def productFoldLeft(ds: MyList[Double]): Double = foldLeft(ds, 1.0)(_ * _)
  def lengthFoldLeft[A](l: MyList[A]): Int = foldLeft(l, 0)((_, acc) => 1 + acc)

  // 3.12 reverse
  def reverseFoldLeft[A](l: MyList[A]): MyList[A] =
    foldLeft[A, MyList[A]](l, Nil)((el, acc) => append(Cons(el, Nil), acc))

  def reverseFoldRight[A](l: MyList[A]): MyList[A] =
    foldRight[A, MyList[A]](l, Nil)((el, acc) => append(acc, Cons(el, Nil)))

  // 3.13 hard T__T
  //  def foldRightTailRec[A, B](l: MyList[A], z: B)(f: (A, B) => B) =

  // 3.14
  def appendFoldRight[A](l1: MyList[A], l2: MyList[A]): MyList[A] =
    foldRight(l1, l2)((el, acc) => Cons(el, acc))

  // TODO: is it possible to implement using foldLeft?
  //  def appendFoldLeft[A](l1: MyList[A], l2: MyList[A]): MyList[A] =

  // 3.15
  def flattenList[A](ll: MyList[MyList[A]]): MyList[A] =
    ll match {
      case Nil => Nil
      case Cons(h, t) => foldLeft(t, h)((list, acc) => append(acc, list))
    }

  // 3.16
  // 3.18
  def map[A, B](l: MyList[A])(fn: A => B): MyList[B] =
    l match {
      case Nil => Nil
      case Cons(x, t) => Cons(fn(x), map(t)(fn))
    }

  def filter[A](l: MyList[A])(predicate: A => Boolean): MyList[A] =
    l match {
      case Nil => Nil
      case Cons(x, t) => if(predicate(x)) Cons(x, filter(t)(predicate)) else filter(t)(predicate)
    }

  // 3.20
  def flatMap[A, B](l: MyList[A])(fn: A => MyList[B]): MyList[B] =
    l match {
      case Nil => Nil
      case Cons(x, t) => append(fn(x), flatMap(t)(fn))
    }

  // 3.21
  def filterWithFlatMap[A](l: MyList[A])(predicate: A => Boolean): MyList[A] =
    flatMap(l)(x => if (predicate(x)) MyList(x) else Nil)

  // 3.22 and 3.23
  def zipWith[A, B](l1: MyList[A], l2: MyList[A])(fn: (A, A) => B): MyList[B] =
    (l1, l2) match {
      case (Nil, Nil) => Nil
      case (Cons(x, t1), Cons(y, t2)) => Cons(fn(x, y), zipWith(t1, t2)(fn))
    }

  @tailrec
  def contains[A](l: MyList[A], el: A): Boolean =
    l match {
      case Nil => false
      case Cons(x, t) => x == el || contains(t, el)
    }

  def indexOf[A](l: MyList[A], el: A): Int = {
    @tailrec
    def loop(l: MyList[A], acc: Int): Int = l match {
      case Nil => -1
      case Cons(x, t) => if(x == el) acc else loop(t, acc + 1)
    }
    loop(l, 0)
  }

  // 3.24
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {

    @tailrec
    def loop(sup: MyList[A], sub: MyList[A], lastIdx: Int): Boolean =
      if(lastIdx == -1) false
      else sub match {
        case Nil => true
        case Cons(x, t) => {
          val currentIdx = indexOf(sup, x)
          ((lastIdx + 1) == currentIdx) && loop(sup, t, currentIdx)
        }
      }

    sub match {
      case Nil => true
      case Cons(x, t) => loop(sup, t, indexOf(sup, x))
    }
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
