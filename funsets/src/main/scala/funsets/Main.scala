package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))

  val mod_two = (x: Int) => (x % 2) == 0
  val mod_two_mult_two = map(mod_two, (x: Int) => x * 2)


  printSet(mod_two)
  printSet(mod_two_mult_two)
}
