package com.gashu.myfpinscala.chapter6

/**
 * @author tiagogashu in 29/12/2019
 **/
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

}

object StateMachine extends App {

}
