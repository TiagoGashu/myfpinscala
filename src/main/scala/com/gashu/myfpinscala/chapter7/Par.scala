package com.gashu.myfpinscala.chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

import com.gashu.myfpinscala.parallelism.Actor

/**
 * @author tiagogashu in 29/12/2019
 **/
sealed trait Future[A] {
  private[chapter7] def apply(k: A => Unit): Unit
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    _ => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {
      a => ref.set(a); latch.countDown()
    }
    latch.await
    ref.get
  }

  def map2[A,B,C](p: Par[A], p2: Par[B])(f: (A,B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }
          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    (x: A) => fork[B](unit(f(x)))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  // 7.5
  def sequenceBalanced[A](as: List[Par[A]]): Par[List[A]] = fork {
    as match {
      case Nil => unit(List())
      case h :: Nil => map(h)(List(_))
      case list =>
        val (l, r) = list.splitAt(list.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as))(_.toList)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] =
    sequence(ps.map(asyncF(f)))

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    as match {
      case Nil => unit(List())
      case h :: Nil => if(f(h)) unit(List(h)) else unit(List())
      case list =>
        val (l, r) = list.splitAt(list.length/2)
        map2(parFilter(l)(f), parFilter(r)(f))(_ ++ _)
    }
  }

  // 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => choices(run(es)(n))(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if(_) 1 else 0))(List(f, t))

  // 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
    es => choices(run(es)(key))(es)

  // 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa))(es)

  def choiceWithChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser[Int, A](map(cond)(if(_) 1 else 0))(List(f, t)(_))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser[Int, A](n)(choices(_))

  // TODO: generalize the sum fn

  /**
   * TODO:
   *
   * Write a function that takes a list of paragraphs (a List[String] ) and returns
   *  the total number of words across all paragraphs, in parallel. Generalize this
   *  function as much as possible.
   *
   */

  // TODO: Implement map3 , map4 , and map5 , in terms of map2.

}
