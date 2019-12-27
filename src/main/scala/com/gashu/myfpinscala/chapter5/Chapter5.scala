package com.gashu.myfpinscala.chapter5

/**
 * @author tiagogashu in 27/12/2019
 **/
object Chapter5 extends App {

  val streamOfInts: Stream[Int] = Stream(1, 2, 3)

  println(streamOfInts.toList)

}
