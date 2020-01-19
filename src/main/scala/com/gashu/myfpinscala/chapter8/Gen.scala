package com.gashu.myfpinscala.chapter8

import com.gashu.myfpinscala.chapter6.{RNG, State}

/**
 * @author tiagogashu in 17/01/2020
 **/
case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen {
      State {
        r => {
          val (v, r2) = sample.run(r)
          (f(v), r2)
        }
      }
    }

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen {
      State {
        r => {
          val (v, r2) = sample.run(r)
          f(v).sample.run(r2)
        }
      }
    }

  def listOfN(sizeGen: Gen[Int]): Gen[List[A]] =
    sizeGen.flatMap(size => Gen.listOfN(size, this))

  def unsized[A] = SGen(_ => this)

}

case class SGen[A](forSize: Int => Gen[A]) {

  def map[B](f: A => B): SGen[B] =
    SGen {
      x => forSize(x) map(f)
    }

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen {
      x => forSize(x) flatMap(f)
    }

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen {
      State {
        (rng: RNG) => {
          val (n, rng2) = RNG.nonNegativeInt(rng)
          val nBetween = (n % (stopExclusive - start)) + start
          (nBetween, rng2)
        }
      }
    }

  def unit[A](a: => A): Gen[A] =
    Gen {
      State {
        r => { (a, r) }
      }
    }

  def boolean: Gen[Boolean] =
    Gen {
      State {
        r => {
          val (n, rng2) = r.nextInt
          val (n2, rng3) = rng2.nextInt
          (n < n2, rng3)
        }
      }
    }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {

    def loop(r: RNG, n: Int, g: Gen[A]): (List[A], RNG) = {
      if (n == 0) (List(), r)
      else {
        val (v, r2) = g.sample.run(r)
        val (listOfV, retRng) = loop(r2, n - 1, g)
        (v :: listOfV, retRng)
      }
    }

    Gen {
      State {
        r => loop(r, n, g)
      }
    }
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen {
      State {
        r => {
          val (d, r2) = RNG.double(r)
          if (d < 0.5) g1.sample.run(r2)
          else g2.sample.run(r2)
        }
      }
    }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    Gen {
      State {
        r => {
          val (d, r2) = RNG.double(r)
          val totalWeight = g1._2 + g2._2
          val g1Weight = g1._2 / totalWeight
          if(d < g1Weight) g1._1.sample.run(r2)
          else g2._1.sample.run(r2)
        }
      }
    }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen {
      x => Gen.listOfN(x, g)
    }

}