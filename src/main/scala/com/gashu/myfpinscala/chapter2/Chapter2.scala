package com.gashu.myfpinscala.chapter2

/**
 * @author tiagogashu in 27/10/2019
 **/
object Chapter2 extends App {

  import ScalaExercisesChapter2._

  // chapter 2
  println(fib(5))

  println(isSorted[Int](Array(1, 2, 3, 1), (x, y) => x <= y))

  def sum(a: Int, b: Int) = a + b

  // fixing the "a" param to 1
  def sumOne(x: Int): Int = {
    val curriedSum: Int => Int = curry(sum)(1)
    curriedSum(x)
  }

  sumOne(2)

  def sumConstant(cons: Int) = curry(sum)(cons)

  uncurry(sumConstant)(1, 2)

}
