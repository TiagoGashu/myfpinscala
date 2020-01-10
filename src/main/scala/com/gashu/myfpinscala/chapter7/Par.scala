package com.gashu.myfpinscala.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

/**
 * @author tiagogashu in 29/12/2019
 **/
private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
  (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  // 7.4
  def asyncF[A, B](f: A => B): A => Par[B] =
    (x: A) => fork[B](unit(f(x)))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldLeft[Par[List[A]]](unit(List()))((listOfPar, par) => {
      map2(listOfPar, par)((listOfA, a) => a :: listOfA)
    })

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {

    def filterFoldingRight(a: A, list: List[A]):List[A] = if(f(a)) a :: list else list

    as.foldRight[Par[List[A]]](unit(List()))((a, listOfPar) => {
      map2(unit(a), listOfPar)(filterFoldingRight)
    })

  }
}
