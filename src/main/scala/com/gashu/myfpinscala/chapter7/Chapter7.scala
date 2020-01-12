package com.gashu.myfpinscala.chapter7

import java.util.concurrent.Executors

import com.gashu.myfpinscala.chapter7.Par.Par

/**
 * @author tiagogashu in 29/12/2019
 **/
object Chapter7 extends App {

  import com.gashu.myfpinscala.chapter7.Par._

  def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

  val p = parMap(List.range(1, 100000))(math.sqrt(_))
  val x = run(Executors.newFixedThreadPool(2))(p)

  val y = run(Executors.newFixedThreadPool(2))(parFilter(List.range(1, 10000))(_ % 2 == 0))

//  println(x.head)
  println(y.head)

}
