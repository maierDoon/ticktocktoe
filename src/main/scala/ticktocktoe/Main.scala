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

  def stopGame(me: Player, oponent: Player): Boolean = {
    val spots = me.getSpots

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
      oponent.getSpots.length + spots.length == 9

    if (isAnyRowFilled || isAnyColumnFilled || isDiagonalFilled) {
      println(me.name + " is a winner !!")
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
   findSpot(playerOne, playerTwo)
      /*val col = random.nextInt(3)
      val row = random.nextInt(3)
      (row, col)*/
  }


  def findSpot(playerOne: Player, playerTwo: Player): Coord = {

    def needToDefend(line: Int, isRow: Boolean): Option[Coord] = {
      val func = (c: Coord, line: Int) =>  if(isRow) c._1 == line else c._2 == line

      if (playerOne.getSpots.count(x => func(x, line)) == 2 &&
        !playerTwo.getSpots.exists(x => func(x, line))) {
        if(isRow){
          Some(playerOne.getSpots.filter(x => func(x, line)).fold((line, 3))((x, y) => (line, x._2 - y._2)))  //reduce{(x, y) => (x._1, x._2 + y._2)}
        }else {
          Some(playerOne.getSpots.filter(x => func(x, line)).fold((3, line))((x, y) => (x._1 - y._1, line)))
        }
      } else
        None
    }

  def defendRightDiagonal: Option[Coord] = {
    if(playerOne.getSpots.count(spot => spot._1 == 2 - spot._2) == 2 &&
      playerTwo.getSpots.count(spot => spot._1 == 2 - spot._2) == 0) {
      Some(playerOne.getSpots.filter(spot => spot._1 == 2 - spot._2).fold((3, 3))((old, current) => (old._1 - current._1, old._2 - current._2)))
    } else {
      None
    }
  }

  def defendLeftDiagonal: Option[Coord] = {
    if(playerOne.getSpots.count(spot => spot._1 == spot._2) == 2 &&
      playerTwo.getSpots.count(spot => spot._1 == spot._2) == 0){
      Some(playerOne.getSpots.filter(spot => spot._1 == spot._2).fold((3, 3))((old, current) => (old._1 - current._1, old._2 - current._2)))
    }else{
      None
    }
  }


    needToDefend(0, isRow = true).
      getOrElse(needToDefend(1, isRow = true).
        getOrElse(needToDefend(2, isRow = true).
          getOrElse(needToDefend(0, isRow = false).
            getOrElse(needToDefend(1, isRow = false).
              getOrElse(needToDefend(2, isRow = false).
                getOrElse(defendRightDiagonal.
                  getOrElse(defendLeftDiagonal.
                   getOrElse(random.nextInt(3),random.nextInt(3)))))))))

  }

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

    if (!stopGame(personOne, playerTwo)) {
      println(playerTwo.name + ", please go")
      val personTwo = playerTwoAttempt(personOne, playerTwo)
      printBoardToConsole(personOne, personTwo)

      if (!stopGame(personTwo, personOne))
        playing(personOne, personTwo)
    }
  }


  printBoardToConsole(playerOne, playerTwo)
  playing(playerOne, playerTwo)
}

