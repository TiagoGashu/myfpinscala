package com.gashu.myfpinscala.chapter7

/**
 * @author tiagogashu in 29/12/2019
 **/
object Chapter7 extends App {

  def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

}
