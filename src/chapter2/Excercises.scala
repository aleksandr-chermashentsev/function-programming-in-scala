package chapter2

import scala.annotation.tailrec

/**
  *
  * @author a.chermashentsev
  *         Date: 17.01.2019
  **/
object Excercises extends App {

  // 2.1
  //Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
  //The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
  //previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
  //local tail-recursive function.
  def fib(n: Int): Int = {
    @tailrec
    def impl(prev: Int, current: Int, n: Int): Int =
      if (n == 2) current
      else impl(current, current + prev, n - 1)

    if (n == 1) 0
    else impl(0, 1, n)
  }
  println("====FIBONACCI START====")
  for(i <- 1 to 10) {
    println(fib(i))
  }
  println("====FIBONACCI END====")

  // 2.2
  //Implement isSorted, which checks whether an Array[A] is sorted according to a
  //given comparison function:
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    (as, as.drop(1)).zipped.forall(ordered)
  }

  println("====IS SORTED START====")
  println(isSorted(Array(1,2,3), (a1:Int, a2:Int) => a1 >= a2))
  println(isSorted(Array(1,2,3), (a1:Int, a2:Int) => a1 <= a2))
  println(isSorted(Array(1,3,2,3), (a1:Int, a2:Int) => a1 <= a2))
  println("====IS SORTED END====")

  // 2.3
  //Let’s look at another example, currying,
  //which converts a function f of two arguments
  //into a function of one argument that partially applies f. Here again there’s only one
  //implementation that compiles. Write this implementation.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    A => B => f(A, B)
  }

  // 2.4
  //Implement uncurry, which reverses the transformation of curry. Note that since =>
  //associates to the right, A => (B => C) can be written as A => B => C.
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (A, B) => f(A)(B)
  }

  // 2.5
  //Implement the higher-order function that composes two functions
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    A => f(g(A))
}
