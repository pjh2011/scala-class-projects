package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)) === true, "0,0")
      assert(terrain(Pos(0,1)) === true, "0,0")
      assert(terrain(Pos(0,2)) === true, "0,0")
      assert(!terrain(Pos(0,3)) === true, "0,0")
      assert(!terrain(Pos(0,4)) === true, "0,0")
      assert(!terrain(Pos(0,5)) === true, "0,0")
      assert(!terrain(Pos(0,6)) === true, "0,0")
      assert(!terrain(Pos(0,7)) === true, "0,0")
      assert(!terrain(Pos(0,8)) === true, "0,0")
      assert(!terrain(Pos(0,9)) === true, "0,0")
      assert(!terrain(Pos(0,10)) === true, "0,0")
      assert(terrain(Pos(1,1)) === true, "1,1") // start
      assert(terrain(Pos(4,7)) === true, "4,7") // goal
      assert(terrain(Pos(5,8)) === true, "5,8")
      assert(!terrain(Pos(5,9)) === true, "5,9")
      assert(terrain(Pos(4,9)) === true, "4,9")
      assert(!terrain(Pos(6,8)) === true, "6,8")
      assert(!terrain(Pos(4,11)) === true, "4,11")
      assert(!terrain(Pos(-1,0)) === true, "-1,0")
      assert(!terrain(Pos(0,-1)) === true, "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }


	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
