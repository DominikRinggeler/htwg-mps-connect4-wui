package mps.Model

/**
 * Created by dominikringgeler on 06.01.16.
 */
object GameStatus extends Enumeration {
  type GameStatus = Value
  val WIN, DRAW, PLAYING, NOT_PLAYING = Value
}