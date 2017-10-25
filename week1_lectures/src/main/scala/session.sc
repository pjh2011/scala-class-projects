import scala.annotation.tailrec

object session {
  1 + 3
  def abs(x: Double) = if (x < 0) -x else x
  abs(-1-4)



  def sqrt(x: Double): Double = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) / x <   0.001

    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2


    sqrtIter(1.0, x)
  }

  sqrt(1e20)
  1e60+1

  def factorial(n: Int): Int = {
    @tailrec
    def loop(i: Int, acc: Int): Int = {
      if (i == 0) acc
      else loop(i-1, acc * i)
    }

    loop(n, 1)
  }

  factorial(5)
}
