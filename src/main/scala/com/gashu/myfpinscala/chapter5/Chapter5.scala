package com.gashu.myfpinscala.chapter5

/**
 * @author tiagogashu in 27/12/2019
 **/
object Chapter5 extends App {

  val streamOfInts: Stream[Int] = Stream(1, 2, 3)
  val otherStreamOfInts: Stream[Int] = Stream(4, 5, 6)

//  println(streamOfInts.toList)

//  println(streamOfInts.takeWhileUsingFoldRight(_ < 3).toList)

//  println(streamOfInts.headOption)

//  println(Empty.headOption)

//  println(Stream.concat(streamOfInts, otherStreamOfInts).toList)

  println(streamOfInts.flatMap(x => Stream(x, x + 1, x + 2)).toList)

}
