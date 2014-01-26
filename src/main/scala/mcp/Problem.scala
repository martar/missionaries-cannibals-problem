package mcp

import collection.immutable.Queue
import collection.immutable.HashSet

class Problem( initialState : State){

  /**
   * Build the path form the end state back to the first state (in order to have the answer and not only know that we found the answer)
   */
  def pathTo(state: State): List[State] = {
    /*
     * Every state has a parent. You can thing of it as a tree. So we build the path by going up in the hierarchy, until we
     * reach the state whose parent is null (root)
     */
    def pathTo1(state: State, path: List[State]): List[State] = state.parent match {
      case null => state :: path
      case _ => pathTo1(state.parent, state :: path)
    }

    pathTo1(state, List[State]())
  }

  def eval(possibleStates: List[State], alreadyCheckedStates: HashSet[State]): List[State] = possibleStates match {
    case Nil => null // if there is no possible states yet we cannot do any more,the goal was not reached
    case head :: tail => {
      if (head.goalTest) {
        println("Mission completed!")
        pathTo(head)
      } else {
        /*
         * Current state is not the goal state so we seek what are the feasible states we can transform our current state
         * we also know what states we checked so far: alreadyCheckedStates
         * so we want to have filtered list of new states that we havent's visited yet and are feasible to transform from current state (misioneries not eaten etc)
         * to alreadyCheckedStates we add the state that we checked now that id not a goal, and take off this state from the possibleStates list.
         * new distincs possible states found by expantion are now added to possibleStates list (ok I know it would be better if it was a set). Of caurse we 
        */
        val updatedAlreadyCheckedStates = alreadyCheckedStates + head
        eval((tail ++ head.expand.filter(x => !updatedAlreadyCheckedStates.contains(x))).distinct, updatedAlreadyCheckedStates)
      }

    }

  }

  /**
   * We're starting. Boat is on the left bank. On the left bank there are also 3 cannibals and 3 misionaries. This is the first possible state
   * The set of already checked states is empty
   */
  def start = eval(List[State](initialState), new HashSet[State])
}

/**
 * Action type defines the transformation form one state to another
 */
abstract class Action(val numCannibals: Int, val numMissionaries: Int) {
  /*
   * Transfom the given state s into a new state. Mind that the parent of the new state is an old state
   */
  def transform(s: State): State
}

/**
 * Right to Left Action defines the action of traversing the river in the boat from right to left bank of the river
 */
class RightToLeftAction(numCannibals: Int, numMissionaries: Int) extends Action(numCannibals, numMissionaries) {
  def transform(s: State): State = {
    new BoatOnLeftState(s.numCLeft + numCannibals, s.numMLeft + numMissionaries, s)
  }
}

/**
 * Left to Right Action defines the action of traversing the river in the boat from left to right bank of the river
 */
class LeftToRightAction(numCannibals: Int, numMissionaries: Int) extends Action(numCannibals, numMissionaries) {
  def transform(s: State): State = {
    new BoatOnRightState(s.numCLeft - numCannibals, s.numMLeft - numMissionaries, s)
  }

}

abstract class State(val numCLeft: Int, val numMLeft: Int, val parent: State) {

  val overallNumber = 3 //overall number of missionares equal to overall number to cannibals in out world

  /*
   * return the new feasible states that results of a current state
   */
  def expand: List[State]

  /*
   * Whether the state is a state that we're looking for
   */
  def goalTest: Boolean = false

  /*
   * Check whether in current state, the missionaries stayes alive - so if the state fulfill the requirement
   */
  def missionariesNotEaten = (numCLeft <= numMLeft || numMLeft == 0) && (numCLeft >= numMLeft || numMLeft == 3)

  /*
   * For a given list of actions, check if the action is possible - so whether for example if 
   * action requires two missionaries to cross the river, there is at least two missionaries on the bank where they start the jurney.
   * Then, apply the transformation to the current state. Then check if the resulting state is valid (missionaries are not eaten)
   * 
   */
  def transformAndFilter(list: List[Action], numC: Int, numM: Int): List[State] = {
    list.filter(x => x.numCannibals <= numC && x.numMissionaries <= numM && x.numCannibals + x.numMissionaries <= 2).map(x => x.transform(this)).filter(x => x.missionariesNotEaten)
  }

  override def toString: String = "\n" + this.getClass().toString() + " On the left side: " + numCLeft + " " + numMLeft + ", on the right side: " + (overallNumber - numCLeft) + " " + (overallNumber - numMLeft)

  /*
   * Since, I use a Hash Set for storing the states that was already looked up, I need to override equal and hashCode. Probably there is nicer way to do it.
   */
  override def equals(o: Any) = o match {
    case that: State => that.getClass().equals(this.getClass()) && that.numCLeft == this.numCLeft && that.numMLeft == this.numMLeft
    case _ => false
  }
  override def hashCode = (numCLeft + ";" + numMLeft + ";" + this.getClass().toString()).hashCode()

}

class BoatOnLeftState(override val numCLeft: Int, override val numMLeft: Int, override val parent: State) extends State(numCLeft, numMLeft, parent) {
  override def expand: List[State] = {
    /**
     * I'm manualy listing all possible combinations of the boat crew. Probably can be done much nicer
     */
    val actions = List[Action](new LeftToRightAction(1, 0), new LeftToRightAction(2, 0), new LeftToRightAction(1, 1), new LeftToRightAction(0, 2), new LeftToRightAction(0, 1))
    transformAndFilter(actions, numCLeft, numMLeft)
  }

}

/* 
 * numbers of cannibals and missionares on the Left bank defines the number of them both on the right bank two, cuz the overall number stays constant)
 */
class BoatOnRightState(override val numCLeft: Int, override val numMLeft: Int, override val parent: State) extends State(numCLeft, numMLeft, parent) {
  override def expand: List[State] = {
    val actions = List[Action](new RightToLeftAction(1, 0), new RightToLeftAction(2, 0), new RightToLeftAction(1, 1), new RightToLeftAction(0, 2), new RightToLeftAction(0, 1))
    transformAndFilter(actions, overallNumber - numCLeft, overallNumber - numMLeft)

  }
  /*
   * The goal is achiewed once all missionaries and cannibals are on the right bank and the boat is on the right bank
   */
  override def goalTest: Boolean = {
    return numMLeft == 0 && numCLeft == 0
  }
}



