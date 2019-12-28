package com.gashu.myfpinscala.chapter6

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

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = rng.nextInt
    val doubleN = if(n < 0)
      (math.abs(n + 1) / Int.MaxValue).toDouble
      else (n / Int.MaxValue).toDouble
    (doubleN, nextRNG)
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

}