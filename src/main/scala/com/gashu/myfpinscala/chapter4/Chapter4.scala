package com.gashu.myfpinscala.chapter4

/**
 * @author tiagogashu in 30/11/2019
 **/
object Chapter4 extends App {

  def mean(xs: Seq[Double]): MyOption[Double] =
    if(xs.isEmpty) Empty
    else Option(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

}
