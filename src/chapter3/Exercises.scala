package chapter3

import scala.annotation.tailrec

/**
  *
  * @author a.chermashentsev
  *         Date: 22.01.2019
  **/
object Exercises extends App {

  sealed trait List[+A] {

    // 3.2 implement tail
    def tail(): List[A] = this match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    //3.4 implement drop
    def drop(n: Int): List[A] = {
      def impl(n: Int, l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_, xs) =>
          if (n > 1) impl(n - 1, xs)
          else xs
      }

      impl(n, this)
    }

    def dropWhile(f: A => Boolean): List[A] = {
      def impl(f: A => Boolean, l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) =>
          if (f(x)) impl(f, xs)
          else Cons(x, xs)
      }

      impl(f, this)
    }

    def init(): List[A] = {
      def impl(l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, impl(xs))
      }

      impl(this)
    }

  }

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, t: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //3.3
    def setHead[A](newHead: A, list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(_, xs) => Cons(newHead, xs)
    }

  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => 1 + b)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def impl(as: List[A], res: B): B = {
      as match {
        case Nil => res
        case Cons(x, xs) => impl(xs, f(res, x))
      }
    }

    impl(as, z)
  }

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  println(x)

  println(List(1, 2, 3, 4, 5).tail())
  println(List.setHead(7, List(1, 2, 3, 4, 5)))
  println(List(1, 2, 3, 4, 5).drop(2))
  println(List(1, 2, 3, 4, 5).dropWhile(x => x < 3))
  println(List(1, 2, 3, 4, 5).init())

  println(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
  println(length(List(1, 2, 3, 4, 5, 6)))
  println(foldLeft(List(1,2,3,4,5), 1)(_+_))
}
