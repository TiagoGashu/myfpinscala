package com.gashu.myfpinscala.chapter8

import com.gashu.myfpinscala.chapter6.{RNG, SimpleRNG}

/**
 * @author tiagogashu in 19/01/2020
 **/
object Chapter8 extends App {

  val smallInt = Gen.choose(1,10)
  val maxProp = Prop.forAll(Gen.listOf(smallInt)) {
    ns =>
      if(ns.isEmpty) true else {
        val max = ns.max
        !ns.exists(_ > max)
      }
  }

  Prop.run(maxProp)

  val sortMinProp = Prop.forAll(Gen.listOf(smallInt)) {
    ns =>
      if(ns.isEmpty) true
      else ns.sorted.head == ns.min
  }
  val sortMaxProp = Prop.forAll(Gen.listOf(smallInt)) {
    ns =>
      if(ns.isEmpty) true
      else ns.sorted.last == ns.max
  }

  Prop.run(sortMinProp)
  Prop.run(sortMaxProp)

}
