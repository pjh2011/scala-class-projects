package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() === MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() === MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() === MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("polynomial determinant correct") {
    val a: Double = 1
    val b: Double = 2
    val c: Double = 3

    val a_signal = Signal(a)
    val b_signal = Signal(b)
    val c_signal = Signal(c)

    val det_sign = Polynomial.computeDelta(a_signal, b_signal, c_signal)

    assert(det_sign.apply == scala.math.pow(b, 2) - 4 * a * c)
  }

  test("polynomial roots correct1") {
    val a: Double = 1
    val b: Double = 0
    val c: Double = -1

    val a_signal = Signal(a)
    val b_signal = Signal(b)
    val c_signal = Signal(c)

    val det_signal = Polynomial.computeDelta(a_signal, b_signal, c_signal)

    assert(det_signal.apply === scala.math.pow(b, 2) - 4 * a * c)

    val solns = Set(1, -1)

    assert(Polynomial.computeSolutions(a_signal, b_signal, c_signal, det_signal).apply() === solns)
  }

  test("polynomial roots correct2") {
    val a: Double = 1
    val b: Double = 0
    val c: Double = 0

    val a_signal = Signal(a)
    val b_signal = Signal(b)
    val c_signal = Signal(c)

    val det_signal = Polynomial.computeDelta(a_signal, b_signal, c_signal)

    assert(det_signal.apply === scala.math.pow(b, 2) - 4 * a * c)

    val solns = Set(0)

    assert(Polynomial.computeSolutions(a_signal, b_signal, c_signal, det_signal).apply() === solns)
  }

  test("polynomial roots correct3") {
    val a: Double = 1
    val b: Double = 4
    val c: Double = 4

    val a_signal = Signal(a)
    val b_signal = Signal(b)
    val c_signal = Signal(c)

    val det_signal = Polynomial.computeDelta(a_signal, b_signal, c_signal)

    assert(det_signal.apply === scala.math.pow(b, 2) - 4 * a * c)

    val solns = Set(-2.0)

    assert(Polynomial.computeSolutions(a_signal, b_signal, c_signal, det_signal).apply() === solns)
  }

  test("polynomial roots correct4") {
    val a: Double = 1
    val b: Double = 0
    val c: Double = 4

    val a_signal = Signal(a)
    val b_signal = Signal(b)
    val c_signal = Signal(c)

    val det_signal = Polynomial.computeDelta(a_signal, b_signal, c_signal)

    assert(det_signal.apply === scala.math.pow(b, 2) - 4 * a * c)

    val solns = Set()

    assert(Polynomial.computeSolutions(a_signal, b_signal, c_signal, det_signal).apply() === solns)
  }

}
