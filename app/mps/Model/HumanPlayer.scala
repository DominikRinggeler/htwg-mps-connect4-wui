package mps.Model

/**
 * Created by dominikringgeler on 12.10.15.
 */
class HumanPlayer (val color: Int, val name: String){
  val token = new GameToken(this)
}