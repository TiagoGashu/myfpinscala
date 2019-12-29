package com.gashu.myfpinscala.chapter6

/**
 * @author tiagogashu in 29/12/2019
 **/
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object StateMachine extends App {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    val listOfStates = inputs.map(input =>

      State[Machine, (Int, Int)](machine => {

        input match {

          case Coin =>
            val updatedCoins = machine.coins + 1
            (
              (updatedCoins, machine.candies),
              Machine(locked = !(machine.candies > 0 && machine.locked), machine.candies, updatedCoins)
            )
          case Turn =>
            if (machine.locked)
              ((machine.coins, machine.candies),
                machine)
            else {
              val updatedCandies = machine.candies - 1
              ((machine.coins, updatedCandies),
                Machine(locked = true, updatedCandies, machine.coins))
            }

        }

      })

    )

    State.sequence(listOfStates).map(l => l.last)
  }

  val inputs = List(Coin, Turn)
  val machine = Machine(locked = true, 2, 0)
  val finalState = simulateMachine(inputs).run(machine)
  println(finalState)

}
