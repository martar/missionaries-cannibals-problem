package mcp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class ProblemTests extends FunSuite {
  test("Goal state correct") {
    val state = new BoatOnRightState(0, 0, null)
    assert(state.goalTest)
  }
  test("Goal state not correct Boat on left side") {
    val state = new BoatOnLeftState(0, 0, null)
    assert(!state.goalTest)

  }
  test("Goal state not correct") {
    val state = new BoatOnRightState(1, 0, null)
    assert(!state.goalTest)
    val state2 = new BoatOnRightState(1, 1, null)
    assert(!state2.goalTest)
    val state3 = new BoatOnRightState(3, 3, null)
    assert(!state3.goalTest)

  }

  test("Missionaries not eaten") {
    val state3 = new BoatOnRightState(1, 0, null)
    assert(state3.missionariesNotEaten)
    val state4 = new BoatOnRightState(1, 1, null)
    assert(state4.missionariesNotEaten)
    val state5 = new BoatOnRightState(3, 3, null)
    assert(state5.missionariesNotEaten)
  }

  test("Missionaries eaten") {
    val state = new BoatOnRightState(2, 1, null)
    assert(!state.missionariesNotEaten)
    val state2 = new BoatOnRightState(3, 2, null)
    assert(!state2.missionariesNotEaten)
  }

  test("Moving from right to left bank: 1 C, 1 M") {
    // on left bank: 1 M and 1 C it implies 2 M and 2 C on right bank
    val state = new BoatOnRightState(1, 1, null)
    // moving them both to left bank
    val action = new RightToLeftAction(1, 1)
    assert(action.transform(state).numCLeft == 2)
    assert(action.transform(state).numMLeft == 2)
  }

  test("Moving from left to right bank: 1 C, 1 M ") {
    // on left bank: 1 M and 1 C it implies 2 M and 2 C on right bank
    val state = new BoatOnLeftState(1, 1, null)
    // moving them both to right bank
    val action = new LeftToRightAction(1, 1)
    assert(action.transform(state).numCLeft == 0)
    assert(action.transform(state).numMLeft == 0)
  }

  test("Start the game!") {
    val state = new BoatOnLeftState(3, 3, null)
    val lol = new Problem(state)
    println(lol.start)
  }

}