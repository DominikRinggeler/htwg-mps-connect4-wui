package mps.Model

/**
 * Created by dominikringgeler on 03.01.16.
 */
class Cell(val gameToken: GameToken) {
  def isSet = gameToken!=null
}

object Cell{
  def empty = new Cell(null)
}