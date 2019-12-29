package com.gashu.myfpinscala.chapter6

import scala.annotation.tailrec

/**
 * @author tiagogashu in 28/12/2019
 **/
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  // 6.1
  def nonNegative(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (if(n < 0) -(n + 1) else n, nextRNG)
  }

  def int(rng: RNG): (Int, RNG) = rng.nextInt

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val doubleN = if(n < 0)
      (math.abs(n + 1) / Int.MaxValue).toDouble
      else (n / Int.MaxValue).toDouble
    (doubleN, nextRNG)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (doubleN, rng3) = double(rng2)
    ((n, doubleN), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((n, doubleN), rng2) = intDouble(rng)
    ((doubleN, n), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (doubleN1, rng2) = double(rng)
    val (doubleN2, rng3) = double(rng2)
    val (doubleN3, rng4) = double(rng3)
    ((doubleN1, doubleN2, doubleN3), rng4)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def loop(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count <= 0)
        (xs, rng)
      else {
        val (x, rng2) = r.nextInt
        loop(count - 1, rng2, x :: xs)
      }
    }

    loop(count, rng, List())
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.5
  def doubleUsingMap(rng: RNG): Rand[Double] =
    map(rng => rng.nextInt)(n => {
      if(n < 0) (math.abs(n + 1) / Int.MaxValue).toDouble
      else (n / Int.MaxValue).toDouble
    })

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case rand :: Nil =>
          val (x, rng2) = rand(rng)
          (List(x), rng2)
        case rand :: tail =>
          val (a, rng2) = rand(rng)
          val z = (List(a), rng2)
          tail.foldLeft(z)((combined, nextRand) => {
            val (listA, previousRng) = combined
            val (nextA, nextRng) = nextRand(previousRng)
            (listA ::: List(nextA), nextRng)
          })
      }
    }

  def intsUsingSequence[A](count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(r => r.nextInt))(rng)
}
