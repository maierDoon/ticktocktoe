package ticktocktoe

object Imports {
  type Coord = (Int, Int)
}

import Imports._

/**
  * Created by Maier Lasdun on 6/6/2016.
  */
class Player() {
  var name: String = "computer"
  var isComp: Boolean = false
  private var allSpots: List[Coord] = List()

  def addSpot(spot: Coord): Unit = {
    allSpots = allSpots :+ spot
  }

  def getSpots: List[Coord] =
    allSpots
}


object Main extends App {

  val playerOne = new Player()
  val playerTwo = new Player()

  def emptySpot(attempt: Coord): Boolean = {
    !(playerOne.getSpots.contains(attempt) || playerTwo.getSpots.contains(attempt))
  }

  def printBoardToConsole(): Unit = {

    def spotSymbol(l: Coord): String = {
      if (playerOne.getSpots.contains(l))
        "X"
      else if (playerTwo.getSpots.contains(l))
        "O"
      else " "
    }

    for (x <- 0 to 2) {
      for (y <- 0 to 2) {
        val line: String = if (y != 2) " | " else ""

        print(" " + spotSymbol((x, y)) + line)
      }
      println()

      if (x != 2)
        println("---|----|----")
    }
  }

  def stopGame(player: Player): Boolean = {
    val spots = player.getSpots

    def isRowFilled(row: Int): Boolean =
      (0 to 2).forall(col => spots.contains((row, col)))

    def isAnyRowFilled: Boolean =
      (0 to 2).exists(isRowFilled) // or: (0 to 2).exists(i => isRowFilled(i)) or: (0 to 2) exists isRowFilled

    def isColumnFilled(col: Int): Boolean =
      (0 to 2).forall(row => spots.contains((row, col)))

    def isAnyColumnFilled: Boolean =
      (0 to 2).exists(isColumnFilled)

    def isDiagonalFilled: Boolean =
      (0 to 2).forall(a => spots.contains((a, a))) ||
        (0 to 2).forall(a => spots.contains((a, 2 - a)))

    def isTie: Boolean =
      playerTwo.getSpots.length + playerOne.getSpots.length == 9


    if (isAnyRowFilled || isAnyColumnFilled || isDiagonalFilled) {
      println(player.name + " is a winner !!")
      printBoardToConsole()
      true
    } else if (isTie) {
      println("no winner !!")
      printBoardToConsole()
      true
    } else
      false
  }

  def playerTurn(): Coord = {
    def rowSpot(): Int = {
      val row = scala.io.StdIn.readInt()
      if (row < 3) row
      else {
        println("invalid spot, try again")
        rowSpot()
      }
    }

    def columnSpot(): Int = {
      val column = scala.io.StdIn.readInt()
      if (column < 3) {
        column
      }
      else {
        println("invalid spot, try again")
        columnSpot()
      }
    }

    val row = rowSpot()
    val col = columnSpot()

    if (emptySpot((row, col))) {
      (row, col)
    } else {
      println("spot taken, try again")
      playerTurn()
    }
  }

  def secondPlayer(player: Player): Coord = {
    if (player.isComp) {
      computersTurn()
    } else {
      playerTurn()
    }
  }

  def computersTurn(): Coord = {
    val r = scala.util.Random
    val col = r.nextInt(3)
    val row = r.nextInt(3)

    if (emptySpot((row, col))) {
      (col, row)
    } else computersTurn()
  }

  def introduction(): Unit = {

    def enterName(player: Player): Unit = {
      println("please enter your name")

      def nameSetter(): Unit = {
        val name = scala.io.StdIn.readLine()
        if (name.length > 0) {
          player.name = name
        } else {
          println("name must have at least one letter")
          nameSetter()
        }
      }
      nameSetter()
      //val name = scala.io.StdIn.readLine()
      // player.name = name
    }

    def pickNumOfPlayers(): Unit = {
      println("one player or two players, please enter 1 or 2")
      val numOfPlayers = scala.io.StdIn.readInt()

      numOfPlayers match {
        case 1 => playerTwo.isComp = true
        case 2 =>
          playerTwo.isComp = false
          enterName(playerTwo)
        case _ => pickNumOfPlayers()
      }
    }

    enterName(playerOne)
    pickNumOfPlayers()
  }

  def playing(): Unit = {
    printBoardToConsole()
    println(playerOne.name + ", please go")
    playerOne.addSpot(playerTurn())

    if (!stopGame(playerOne)) {
      printBoardToConsole()
      println(playerTwo.name + ", please go")
      playerTwo.addSpot(secondPlayer(playerTwo))
      if (!stopGame(playerTwo)) {
        playing()
      }
    }
  }

  introduction()
  playing()
}
