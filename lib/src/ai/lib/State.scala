/**
  * Created by rick on 3/1/17.
  */
abstract class State {
    def isGoal: Boolean
    def transition(action: Action): State
    def validActions: List[Action]
}

