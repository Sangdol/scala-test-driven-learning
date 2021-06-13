package algorithm

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable

class PostfixEvaluation extends AnyFunSuite {

  def postfixEvaluationChar(postfix: String): Int = {
    postfix
      .foldLeft(mutable.Stack[Int]()) { (stack, token) =>
        if (token.isDigit)
          stack.push(token.asDigit)
        else {
          val b = stack.pop()
          val a = stack.pop()

          val result = token match {
            case '+' => a + b
            case '-' => a - b
            case '*' => a * b
            case '/' => a / b
          }

          stack.push(result)
        }
      }
      .pop()
  }

  test("postfixEvaluationChar") {
    assert(postfixEvaluationChar("53+") == 8)
    assert(postfixEvaluationChar("513-*") == -10)
    assert(postfixEvaluationChar("813-/") == -4)
  }

  def postfixEvaluation(postfix: List[String]): Double = {
    postfix
      .foldLeft(List[Double]()) { (stack, token) =>
        (stack, token) match {
          case (a :: b :: tail, "+") => (b + a) :: tail
          case (a :: b :: tail, "-") => (b - a) :: tail
          case (a :: b :: tail, "*") => (b * a) :: tail
          case (a :: b :: tail, "/") => (b / a) :: tail
          case (s, token)            => token.toDouble :: s
        }
      }
      .head
  }

  test("postfixEvaluation") {
    assert(postfixEvaluation(List("5", "3", "+")) == 8)
    assert(postfixEvaluation(List("5", "1", "3", "-", "*")) == -10)
    assert(postfixEvaluation(List("8", "1", "3", "-", "/")) == -4)
  }
}
