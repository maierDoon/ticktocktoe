package ticktocktoe

object Imports {
  type Coord = (Int, Int)
}

import Imports._

/**
  * Created by Maier Lasdun on 6/6/2016.
  */
object Player {
  def apply(name: String = "computer", isComp: Boolean = false, spots: List[Coord] = List()): Player = {
    new Player(name, isComp, spots)
  }
}

class Player(val name: String = "computer", val isComp: Boolean = false, private val allSpots: List[Coord] = List()) {
  def getSpots: List[Coord] =
    allSpots

  def addNewSpot(spot: Coord): Player = {
    Player(name, isComp, spot :: allSpots)
  }
}


/*object game {
  val board: List[Coord] =

}*/

object Main extends App {
  val random = new scala.util.Random(0)

  val (playerOne, playerTwo) = createAllPlayers()

  def isEmptySpot(attempt: Coord, playerOne: Player, playerTwo: Player): Boolean = {
    !(playerOne.getSpots.contains(attempt) || playerTwo.getSpots.contains(attempt))
  }

  def printBoardToConsole(playerOne: Player, playerTwo: Player): Unit = {
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
      true
    } else if (isTie) {
      println("no winner !!")
      true
    } else
      false
  }


  def playerTurn(): Coord = {
    def rowSpot(): Int = {
      val row = scala.io.StdIn.readInt()
      if (row < 3)
        row
      else {
        println("invalid spot, try again")
        rowSpot()
      }
    }

    def columnSpot(): Int = {
      val column = scala.io.StdIn.readInt()
      if (column < 3)
        column
      else {
        println("invalid spot, try again")
        columnSpot()
      }
    }

    (rowSpot(), columnSpot())
  }


  def secondPlayerTurn(playerOne: Player, playerTwo: Player): Coord = {
    if (playerTwo.isComp)
      computersTurn(playerOne, playerTwo)
    else
      playerTurn()
  }

  def computersTurn(playerOne: Player, playerTwo: Player): Coord = {

   //needToDefend(playerOne, playerTwo, 0).getOrElse((random.nextInt(3),random.nextInt(3)))


      val col = random.nextInt(3)
      val row = random.nextInt(3)

      (row, col)
  }


  //def needToDefend(playerOne: Player, playerTwo: Player, line: Int): Option[Coord] = {
    /*def row (t: Tuple2) : Boolean =
      t._1 == line

     def col (t: Tuple2) : Boolean =
       t._2 == line*/
      /*if (playerOne.getSpots.count(_._1 == line) == 2 &&
        !playerTwo.getSpots.exists(_._1 == line)) {
        Some(playerOne.getSpots.filter(_._1 == line).fold((line, 3))((x, y) => (line, x._2 - y._2)))  //reduce{(x, y) => (x._1, x._2 + y._2)}
      } else
        None
  }*/

  def createAllPlayers(): (Player, Player) = {
    def createPlayer(): Player = {
      println("please enter your name")

      def nameSetter(): String = {
        val name = scala.io.StdIn.readLine()
        if (name.length > 0)
          name
        else {
          println("name must have at least one letter")
          nameSetter()
        }
      }
      Player(nameSetter())
    }

    def createPlayerTwo(): Player = {
      def isHumanPlayer: Boolean = {
        println("one player or two players, please enter 1 or 2")
        val numberPicked = scala.io.StdIn.readInt()

        numberPicked match {
          case 1 => false
          case 2 => true
          case _ => isHumanPlayer
        }
      }

      if (isHumanPlayer)
        createPlayer()
      else
        Player(name = "computer", isComp = true)
    }

    (createPlayer(), createPlayerTwo())
  }

  def playerOneAttempt(playerOne: Player, playerTwo: Player): Player = {
    val spot = playerTurn()
    if (isEmptySpot(spot, playerOne, playerTwo))
      playerOne.addNewSpot(spot)
    else {
      println("spot taken, try again")
      playerOneAttempt(playerOne, playerTwo)
    }
  }

  def playerTwoAttempt(playerOne: Player, playerTwo: Player): Player = {
    val spot = secondPlayerTurn(playerOne, playerTwo)
    if (isEmptySpot(spot, playerOne, playerTwo))
      playerTwo.addNewSpot(spot)
    else {
      println("spot taken, try again")
      playerTwoAttempt(playerOne, playerTwo)
    }
  }

  def playing(playerOne: Player, playerTwo: Player): Unit = {
    println(playerOne.name + ", please go")
    val personOne = playerOneAttempt(playerOne, playerTwo)
    printBoardToConsole(personOne, playerTwo)

    if (!stopGame(personOne)) {
      println(playerTwo.name + ", please go")
      val personTwo = playerTwoAttempt(personOne, playerTwo)
      printBoardToConsole(personOne, personTwo)

      if (!stopGame(personTwo))
        playing(personOne, personTwo)
    }
  }


  printBoardToConsole(playerOne, playerTwo)
  playing(playerOne, playerTwo)
}

