package utils

import mps.Model.GameStatus.GameStatus
import mps.Model.{GameStatus, Grid, HumanPlayer}
import play.api.mvc.Results

case class OutputField(grid: Grid)

/**
 * Created by dominikringgeler on 23.01.16.
 */
object Wui extends Results {
  def printField(grid: Grid):String = {
    var result = " "

    for (i <- 0 until grid.getColumns) {
      result = result + " " + (i + 1).toString
    }
    result = result +" \n "

    for (rowIndex <- (0 to grid.rows-1)) {
      for (columnIndex <- 0 until grid.getColumns) {
        val cell = grid.getCell(rowIndex,columnIndex)
        val stringVar =
          if(cell==null)
            "  "
          else if(cell.gameToken == null)
            "  "
          else
            cell.gameToken.color

        result = result + "|" + stringVar
      }
      result = result + "|"
      result = result +" \n "
    }
    result = result +" \n "

    result
  }

  def printEndOfTurn(gameStatus: GameStatus, humanPlayer: HumanPlayer): String = {
    gameStatus match {
      case GameStatus.WIN => "\n" + "Das Spiel ist Aus! " + humanPlayer.color + " " +
        "hat gewonnen.  n - Neues Spiel"
      case GameStatus.DRAW => "\n" + "Das Spiel ist Aus! Unentschieden. n - Neues Spiel"
      case GameStatus.PLAYING => " \n "+ humanPlayer.name + " ist an der Reihe, bitte Spalte wählen..."
      case _ => "\n" + "Das Spiel ist Aus! -> n - Neues Spiel, e - Beenden"
    }
  }
}


