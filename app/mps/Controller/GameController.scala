package mps.Controller

import mps.Model.{GameRuleController, GameStatus, Grid, HumanPlayer}
import play.Logger
import utils.Wui

import scala.util.{Try, Success, Failure}

/**
 * Created by dominikringgeler on 25.10.15.
 */
class GameController(var grid:Grid) {
  var players: List[HumanPlayer] = List()
  var nextPlayers: List[HumanPlayer] = List()
  var gameStatus = GameStatus.NOT_PLAYING

  /*
   player control
    */
  def addPlayer(player: HumanPlayer) {
    players = player :: players
    nextPlayers = players
  }

  def removePlayers(): Unit ={
    players = List()
    nextPlayers = players
  }

  // setup game with 2 Players and reset game
  def set2Player(): Unit = {
    removePlayers()

    addPlayer(new HumanPlayer(2,"Spieler 2"))
    addPlayer(new HumanPlayer(1,"Spieler 1"))

    gameStatus = GameStatus.PLAYING

    reset
  }

  // setup game with 3 Players and reset game
  def set3Player(): Unit = {
    removePlayers()

    addPlayer(new HumanPlayer(3,"Spieler 3"))
    addPlayer(new HumanPlayer(2,"Spieler 2"))
    addPlayer(new HumanPlayer(1,"Spieler 1"))

    gameStatus = GameStatus.PLAYING

    reset
  }

  // get the actual player
  def getActualPlayer: HumanPlayer = nextPlayers.head

  // set the next players
  def getNextPlayers: List[HumanPlayer] = {
    nextPlayers = nextPlayers.tail
    if (nextPlayers.length == 0) nextPlayers = players
    nextPlayers
  }

  /*
   grid control
    */
  // reset the grid
  def reset() {
    grid = grid.reset
    gameStatus = GameStatus.PLAYING
    nextPlayers = players
    //publish(new ChangeField)
  }

  def gridRows = grid.rows

  def gridColumns = grid.getColumns

  // get the last row that is free
  def getRowIndex(index:Int) = grid.getRowIndex(index)

  /*
   game control
    */
  // makes a turn if its possible
  def makeTurn(col: Int) = {
    var correct = false
    val actualPlayer = getActualPlayer
    grid.setCell(col, actualPlayer.token) match {
      case Failure(exception) => {
        false
      }
      case Success(value) => {
        grid = value
        if(conn4(col, actualPlayer)) gameStatus = GameStatus.WIN
        true
      }
    }
  }

  // check if the actual player has connect 4
  def conn4(c:Int, player: HumanPlayer): Boolean ={
    val grc = new GameRuleController(grid)
    val win = grc.checkConnectFour(c,player)
    if (!win) {
      nextPlayers = getNextPlayers
    }
    win
  }

  // checks the imputs from the html view input
  def processInputLine(input: String):String = {
    var returnString = ""
    var error = ""

    input match {
      case "n" => reset
      case "p2" => set2Player()
      case "p3" => set3Player()
      case _ => {
        if (gameStatus == GameStatus.PLAYING) {
          var input1 = Try(input.toInt)
          input1 match {
            case Failure(exception) => {
              error = "Nicht gültig, nochmal setzen..." + " \n "
            }
            case Success(i) => i match {
              case i:Int if (i > 0 && i <= gridColumns) => returnString = returnString + printAfterTurn(i)//makeTurnAndCheck(i)
              case i:Int if (i > gridColumns || i <= 0) => error = "Keine Gültige Zahl! " + " \n "
            }
          }
        }
      }
    }

    returnString = returnString + Wui.printField(grid) + error + Wui.printEndOfTurn(gameStatus, getActualPlayer)
    returnString
  }

  def makeTurnAndCheck(input:Int) = {
    var isCorrect = false
    do {
      try {
        val col = input - 1
        if (col >= 0 && col < gridColumns) {
          makeTurn(col)
          isCorrect = true
        }
      } catch {
        case e: Exception =>
      }
    } while (!isCorrect)
  }

  // prints the output of a turn of a player
  def printAfterTurn(input:Int): String = {
    var output = ""
    var isCorrect = false
    try {
      val col = input - 1
      if (col >= 0 && col < gridColumns) {
        isCorrect = makeTurn(col)
        System.out.println("Correct: " + isCorrect)
        if (!isCorrect) output = "Die Eingabe ist keine korrekte Spalte! Bitte Spalte wählen..." + "\n"
      }
    } catch {
      case e: Exception => {
        //printGameField
        output = "Die Eingabe ist keine korrekte Spalte! Bitte Spalte wählen..."
      }
    }
    output
  }
}