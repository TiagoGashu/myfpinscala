package com.gashu.myfpinscala.chapter3

/**
 * @author tiagogashu in 27/10/2019
 **/
object Chapter3 extends App {

  import MyList._
  import MyTree._

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

  println(MyList.map(myL)(_ + 1))

  // 3.17
  println(MyList.map(MyList(1D, 2D, 3D, 4D, 5D))(_.toString))

  // 3.19
  println(filter(MyList(1, 2, 3, 4, 5))(_ % 2 == 0))

  // 3.20
  println(flatMap(MyList(1, 2, 3))(x => MyList(x, x)))

  // 3.21
  println(filterWithFlatMap(MyList(1, 2, 3, 4, 5))(_ % 2 == 0))

  // 3.22 and 3.23
  println(zipWith(MyList(1, 2, 3), MyList(4, 5, 6))(_ + _))

  // 3.24
  println(hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2, 4)))
  println(hasSubsequence(MyList(1, 2, 3, 4), MyList(2, 3)))
  println(hasSubsequence(MyList(1, 2, 3, 4), MyList(2)))
  println(hasSubsequence(MyList(1, 2, 3, 4), MyList(2, 3, 4, 5)))

  // ******** MyTree ********

  // 3.25
  println(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

  // 3.26
  println(maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

  // 3.27
  println(maximumPath(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

  // 3.28
  println(MyTree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1))

  // 3.29
  println(sizeWithFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

  println(maximumWithFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

  println(maximumPathWithFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))))

  println(MyTree.mapWithFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1))

}
