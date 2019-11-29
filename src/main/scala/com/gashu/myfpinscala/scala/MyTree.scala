package com.gashu.myfpinscala.scala

/**
 * @author tiagogashu in 29/11/2019
 **/
sealed trait MyTree[+A]
case class Leaf[A](value: A) extends MyTree[A]
case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

object MyTree {

  // 3.25
  def size[A](t: MyTree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  // 3.26
  def maximum(t: MyTree[Int]): Int =
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  // 3.27
  def maximumPath[A](t: MyTree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => (1 + maximumPath(l)) max (1 + maximumPath(r))
    }

  // 3.28
  def map[A, B](t: MyTree[A])(fn: A => B): MyTree[B] =
    t match {
      case Leaf(v) => Leaf(fn(v))
      case Branch(l, r) => Branch(map(l)(fn), map(r)(fn))
    }

}
