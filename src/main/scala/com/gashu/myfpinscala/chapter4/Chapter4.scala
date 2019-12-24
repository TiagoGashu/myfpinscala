package com.gashu.myfpinscala.chapter4

/**
 * @author tiagogashu in 30/11/2019
 **/
object Chapter4 extends App {

  def mean(xs: Seq[Double]): MyOption[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  println(MyOptionImpl.sequence(List(Some(1), Some(2), Some(3))))
  println(MyOptionImpl.sequence(List(Some(1), Some(2), None)))
  println(MyOptionImpl.sequence(List(Some(1), None, Some(2))))
  println(MyOptionImpl.sequence(List(None, Some(1), Some(2))))

}
