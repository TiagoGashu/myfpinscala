package com.gashu.myfpinscala.chapter8

import com.gashu.myfpinscala.chapter6.{RNG, State}

/**
 * @author tiagogashu in 17/01/2020
 **/
case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(State(r => {
      val (v, r2) = sample.run(r)
      f(v).sample.run(r2)
    }))

  def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
    sizeGen.flatMap(size => Gen.listOfN(size, this))

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(
        (rng: RNG) => {
          val (n, rng2) = rng.nextInt
          val nBetween = (n % (stopExclusive - start)) + start
          (nBetween, rng2)
        }
    ))

  def unit[A](a: => A): Gen[A] =
    Gen(State(r => {
      (a, r)
    }))

  def boolean: Gen[Boolean] =
    Gen(State(r => {
      val (n, rng2) = r.nextInt
      val (n2, rng3) = rng2.nextInt
      (n < n2, rng3)
    }))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {

    def loop(r: RNG, n: Int, g: Gen[A]): (List[A], RNG) = {
      if(n == 0) (List(), r)
      else {
        val (v, r2) = g.sample.run(r)
        val (listOfV, retRng) = loop(r2, n - 1, g)
        (v :: listOfV, retRng)
      }
    }

    Gen(State(r => loop(r, n, g)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(State(r => {
      val (d, r2) = RNG.double(r)
      if(d < 0.5) g1.sample.run(r2)
      else g2.sample.run(r2)
    }))

}