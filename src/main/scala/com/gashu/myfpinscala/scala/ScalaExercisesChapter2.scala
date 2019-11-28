package com.gashu.myfpinscala.scala

import scala.annotation.tailrec

/**
 * @author tiagogashu in 27/10/2019
 **/
class ScalaExercisesChapter2 {}

object ScalaExercisesChapter2 extends App {

  // 2.1
  def fib(n: Int): Int = {

    @tailrec
    def loop(n: Int, a: Int, b: Int): Int =
      if(n == 0) a
      else if (n == 1) b
      else loop(n - 1, b, a + b)

    loop(n, 0, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if(as.length == 1) return true
    val pairs = as.zip(as.drop(1))
    pairs.map {
      case (x, y) => ordered(x, y)
    }
    .reduce (_ & _)
  }

  def partial[A, B, C] (a: A, f: (A, B) => C): B => C = ???

  def partialImpl[A, B, C] (a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    // let us fix a value to param "a"
    def partialFixA(a: A, f: (A, B) => C): B => C =
      (b: B) => f(a, b)
    // fn B => C
    a: A => partialFixA(a, f)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
  // same as f compose g
  // or g andThen f

}
