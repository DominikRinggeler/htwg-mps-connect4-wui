package mps.Model

/**
 * Created by dominikringgeler on 06.01.16.
 */
class GameRuleController (val grid:Grid){
  // check winning situation
  def checkConnectFour(column: Int, humanPlayer: HumanPlayer): Boolean = {
    val rowIndexLastToken = if(0<=grid.getRowIndex(column)) grid.getRowIndex(column)+1 else 0
    var win = checkFourInColumn(rowIndexLastToken, column, humanPlayer.color)

    if (!win) win = checkFourInRow(rowIndexLastToken, column, humanPlayer.color)
    if (!win) win = checkFourDiagonalLeftRightDown(rowIndexLastToken, column, humanPlayer.color)
    if (!win) win = checkFourDiagonalLeftRightUp(rowIndexLastToken, column, humanPlayer.color)
    win
  }

  // check    --XXXX---
  def checkFourInColumn(row:Int, col:Int, currentColor:Int): Boolean ={
    var countToken = 0
    var win = false
    if(row <= (grid.rows-4)) {
      for (rowIndex <- 0 until 4) {
        val tempRow = rowIndex + row
        if (!win) {
          val result = check(col, tempRow, countToken, currentColor)
          win = result._2
          countToken = result._1
        }
      }
    }
    win
  }

  // check      -
  //            X
  //            X
  //            X
  //            X
  //            -
  def checkFourInRow(row:Int, col:Int, currentColor:Int): Boolean ={
    var countToken = 0
    var win = false
    for(colIndex <- -3 until 4){
      val tempCol = col+colIndex
      if (!win) {
        val result = check(tempCol, row, countToken, currentColor)
        win = result._2
        countToken = result._1
      }
    }
    win
  }

  // check
  //
  //          X
  //            X
  //              X
  //                X
  def checkFourDiagonalLeftRightDown(row:Int, col:Int, currentColor:Int): Boolean ={
    var countToken = 0
    var win = false

    for(index <- -3 until 4 ) {
      if (!win) {
        val tmpRow = index + row
        val tempCol = index + col
        if (!win) {
          val result = check(tempCol, tmpRow, countToken, currentColor)
          win = result._2
          countToken = result._1
        }
      }
    }
    win
  }

  // check
  //
  //                X
  //              X
  //            X
  //          X
  def checkFourDiagonalLeftRightUp(row:Int, col:Int, currentColor:Int): Boolean ={
    var countToken = 0
    var win = false
    for(index <- -3 until 4){
      if(!win){
        val tmpRow = (-1*index)+row
        val tempCol = index+col
        if (!win) {
          val result = check(tempCol, tmpRow, countToken, currentColor)
          win = result._2
          countToken = result._1
        }
      }
    }
    win
  }

  def check(col:Int, row:Int, countToken:Int, currentColor:Int) = {
    var win = false
    var reset = true
    var count = countToken
    if(inField(col:Int, row:Int)) {
      val cell = grid.getCell(row, col)
      if(cell!=null && cell.isSet){
        if (currentColor == cell.gameToken.color) {
          reset = false
          count = count + 1
          if (count >= 4)
            win = true
        }
      }
    }
    if(reset) count = 0
    (count, win)
  }

  def inField(col:Int, row:Int) = if(col>=0 && col<grid.getColumns && row>=0 && row<grid.rows) true else false
}