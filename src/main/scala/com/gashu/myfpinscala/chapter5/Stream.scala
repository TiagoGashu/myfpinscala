package com.gashu.myfpinscala.chapter5

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

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        lazy val head = h
        lazy val tail = t
        if( p(head()) ) Cons(head, () => tail().takeWhile(p)) else Empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => if(!p(a)) false else b)

  // 5.5
  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if(p(a)) Stream.cons(a, b) else Empty)

  // 5.6
  def headOption: Option[A] =
    foldRight[Option[A]](None)((a, _) => if(a != Empty) Some(a) else None)

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, b) => if(f(a)) Stream.cons(a, b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((a, b) => Stream.concat(f(a), b))

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

  def concat[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    s1 match {
      case Empty => s2
      case Cons(h, t) => Cons(h, () => concat(t(), s2))
    }

  // 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  // 5.10
  def fib(): Stream[Int] = {
    def aux(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, aux(b, a + b))

    aux(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some((value, newState)) => Stream.cons(value, unfold(newState)(f))
    }
  }

  // 5.12
  def unfoldConstant[A](a: A): Stream[A] =
    unfold(a)(s => Some((a, s)))

  def unfoldFrom(n: Int): Stream[Int] =
    unfold(n)(s => Some(s + 1, s + 1))

  def unfoldFib(): Stream[Int] =
    unfold((0, 1))(s => Some(s._2, (s._2, s._1 + s._2)))

  // TODO 5.13, 5.14, 5.15, 5.16

}
