package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(scala.math.pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    if (delta() < 0) Signal(Set())
    else if (delta() == 0) {
      Signal(Set(-b() / (2 * a())))
    } else {
      val pos_soln = (-b() + scala.math.pow(delta(), 0.5)) / (2 * a())
      val neg_soln = (-b() - scala.math.pow(delta(), 0.5)) / (2 * a())
      Signal(Set(pos_soln, neg_soln))
    }
  }
}
