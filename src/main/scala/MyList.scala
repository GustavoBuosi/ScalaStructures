
/*
This class aims to implement a basic List Structure provided by the Scala Immutable collection List.
*/

import java.util.NoSuchElementException
List
abstract class MyList[+A] {
  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
  def map[B](transformer: MyTransformer[A,B]): MyList[B]
  def filter(predicate: MyPredicate[A]): MyList[A]
  def flatMap[B](transformer: MyTransformer[A,MyList[B]]): MyList[B]
  def ++[B >:A] (list: MyList[B]): MyList[B]
}

object Empty extends MyList[Nothing] {
  def head = throw new NoSuchElementException
  def tail = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] = new ListFiller(element, Empty)
  def printElements: String = ""
  def map[B](transformer: MyTransformer[Nothing,B]): MyList[Nothing] = Empty
  def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = Empty
  def flatMap[B](transformer: MyTransformer[Nothing,MyList[B]]): MyList[B] = Empty
  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
}

class ListFiller[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] = new ListFiller(element,this)
  def ++[B >: A](list: MyList[B]): MyList[B] = new ListFiller[B](h, t ++ list)
  override def printElements: String = {
    if (this.t.isEmpty) "" +  h
    else h + " " + t.printElements
  }
  def map[B](transformer: MyTransformer[A,B]): MyList[B] = {
    new ListFiller[B](transformer.converter(h),t.map(transformer))
  }
  def filter(predicate: MyPredicate[A]): MyList[A] = {
    if (predicate.test(h)) new ListFiller[A](h, t.filter(predicate))
    else t.filter(predicate)
  }

  def flatMap[B](transformer: MyTransformer[A,MyList[B]]): MyList[B] = {
    transformer.converter(h) ++ t.flatMap(transformer)
  }

}

abstract class MyPredicate[-T] {
  def test(number: T): Boolean
}

trait MyTransformer[-A,B] {
  def converter(a: A): B
}


object Teste extends App {
  val list: MyList[Int] = new ListFiller(1, new ListFiller(40, new ListFiller(16, new ListFiller(28, Empty))))
  println("Original list: "+ list.toString)

  // Multiplying everything by 2:
  val listMultiplied: MyList[Int] = list.map(new MyTransformer[Int, Int] {
    override def converter(number: Int): Int = number * 2 })

  println("List multiplied by 2: " + listMultiplied.toString)

  val listFiltered = list.filter(new MyPredicate[Int] {
    override def test(number: Int): Boolean = if (number % 2 == 0) true else false
  })

  println("List only with even numbers: " + listFiltered.toString)

  val listExpanded = list.flatMap(new MyTransformer[Int,MyList[Int]] {
    override def converter(a: Int): MyList[Int] = new ListFiller[Int](a, new ListFiller[Int](a+1, Empty))
  })
  println("List with double the size: " + listExpanded.toString)


}
