package fpinscala

import org.scalatest.funsuite.AnyFunSuite

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // my initial version - type order problem
  def fold[A,B](tree: Tree[A])(f: (B,B) => B)(g: A => B): B =
    tree match {
      case Leaf(v) => g(v)
      case Branch(l, r) => f(fold(l)(f)(g), fold(r)(f)(g))
    }

  // type error
  // Leaf(f(v)): Tree[A] => Required Branch[B]
  // Leaf(f(v)): Branch[B] => Cannot upcast Leaf[B] to Branch[B]
//  def foldMap[A,B](tree: Tree[A])(f: A => B): Tree[B] =
//    fold(tree)(Branch(_, _))(v => Leaf(f(v)): Branch[B])

  def fold2[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold2(l)(f)(g), fold2(r)(f)(g))
  }

  def foldMap[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold2(tree)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}

class ch3tree extends AnyFunSuite {
  val t: Branch[Int] = Branch(Leaf(1), Leaf(2))
  val t2: Branch[Int] = Branch(Branch(Leaf(100), Leaf(1)), Leaf(-1))

  test("3.25") {
    assert(Tree.size(t) == 3)
    assert(Tree.size(t2) == 5)
  }

  test("3.26") {
    assert(Tree.maximum(t) == 2)
    assert(Tree.maximum(t2) == 100)
  }

  test("3.27") {
    assert(Tree.depth(t) == 2)
    assert(Tree.depth(t2) == 3)
  }

  test("3.28") {
    assert(Tree.map(t)(_ * 2) == Branch(Leaf(2), Leaf(4)))
  }

  test("3.29") {
    // size
    assert(Tree.fold(t)((a: Int, b: Int) => 1 + a + b)(_ => 1) == 3)

    // maximum
    assert(Tree.fold(t2)((a: Int, b: Int) => a max b)(v => v) == 100)

    // depth
    assert(Tree.fold(t2)((a: Int, b: Int) => 1 + (a max b))(_ => 1) == 3)

    // map
    assert(Tree.foldMap(t)(_ * 2) == Branch(Leaf(2), Leaf(4)))
  }
}
