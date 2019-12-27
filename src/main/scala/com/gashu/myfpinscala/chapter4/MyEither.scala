package com.gashu.myfpinscala.chapter4

import com.gashu.myfpinscala.chapter4.MyOption.sequence

/**
 * @author tiagogashu in 25/12/2019
 **/
sealed trait MyEither[+E, +A] {

  def map[B](f: A => B): MyEither[E, B] =
    this match {
      case Right(value) => Right(f(value))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
    this match {
      case Right(value) => f(value)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
    this match {
      case Right(v) => Right(v)
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C):MyEither[EE, C] =
    this match {
      case Right(v) =>
        b match {
          case Right(bValue) => Right(f(v, bValue))
          case Left(bError) => Left(bError)
        }
      case Left(e) => Left(e)
    }

}

case class Left[+E](error: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]

object MyEither {

  // ex. 4.7
  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = {

    def concatIfNoErrorFound(leftList: List[A], optL: MyEither[E, List[A]]): MyEither[E, List[A]] =
      optL match {
        case Left(e) => Left(e)
        case Right(l) => Right(leftList ::: l)
      }

    es match {
      case Nil => Right(Nil)
      case e: E :: _ => Left(e)
      case Right(x) :: Nil => Right(List(x))
      case Right(x: A) :: (tail: List[MyEither[E, A]]) =>
        (List(List(x)) foldRight sequence(tail)) (concatIfNoErrorFound)
    }
  }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    sequence(as map f)

}