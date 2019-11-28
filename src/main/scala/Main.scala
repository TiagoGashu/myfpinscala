import com.gashu.myfpinscala.scala.MyList

/**
 * @author tiagogashu in 27/10/2019
 **/
object Main extends App {
  import com.gashu.myfpinscala.scala.ScalaExercisesChapter2._
  import com.gashu.myfpinscala.scala.MyList._

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

  // chapter 3
  val myL = MyList(1, 2, 3, 4, 5)

//  println(setHead(myL, 6))
//
//  println(drop(myL, 2))
//
//  println(dropWhile(myL)(_ < 4))
//
//  println(init(myL))
//
//  println(length(myL))

//  println(sumFoldLeft(myL))
//  println(productFoldLeft(MyList(1.0, 2.0, 3.0)))
//  println(length(myL))

//  println(reverseFoldLeft(myL))
//  println(reverseFoldRight(myL))


  println(appendFoldRight(MyList(1, 2, 3), MyList(4, 5, 6)))

  println(MyList(MyList(1, 2, 3), MyList(4, 5, 6), MyList(7, 8, 9)))
  println(flattenList(MyList(MyList(1, 2, 3), MyList(4, 5, 6), MyList(7, 8, 9))))
}
