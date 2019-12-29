package com.gashu.myfpinscala.chapter6

/**
 * @author tiagogashu in 29/12/2019
 **/
case class State[S, +A](run: S => (A, S)) {

  def unit[A](a: A): State[S, A] =
    State(s => (a, s))

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B, C](otherState: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = otherState.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}

object State {

  def sequence[S, A](listOfStates: List[State[S, A]]): State[S, List[A]] =
    State(inputState => {
      listOfStates match {
        case Nil =>
          (Nil, inputState)
        case s :: tail =>
          val (x, generatedState) = s.run(inputState)
          val (xs, finalState) = sequence(tail).run(generatedState)
          (x :: xs, finalState)
      }
    })

}
