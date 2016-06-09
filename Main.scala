package ticktocktoe2

/**
  * Created by Maier Lasdun on 6/6/2016.
  */
class Player() {

  private var name: String = "computer"
  private var isComp: Boolean = false
  private var allSpots: List[List[Int]] = List()

  def setName(n: String): Unit=
    name = n

  def getName(): String =
    name

  def setIsComp(b: Boolean): Unit =
    isComp = b

  def isComputer():Boolean =
    isComp

  def addSpot(spot: List[Int]): Unit = {
    allSpots = allSpots :+ spot
  }

  def getSpots(): List[List[Int]] =
    allSpots




}



object Main extends App {

  val playerOne = new Player()
  val playerTwo = new Player()

  def emptySpot(attempt :List[Int]) : Boolean = {
    !(playerOne.getSpots.contains(attempt) ||  playerTwo.getSpots.contains(attempt))
  }

  def printBoardToConsole(): Unit = {

    def spotSymbal(l: List[Int]): String = {
      if(playerOne.getSpots.contains(l))
        "X"
      else if(playerTwo.getSpots.contains(l))
        "O"
      else " "
    }

    def makeLine(i: Int): String = {
      if(i != 2) " | " else ""
    }

    for(x <- 0 to 2) {
      for(y <- 0 to 2) {
        var line: String = if(y != 2) " | " else ""

        print(" " + spotSymbal(List(x, y)) + line)
      }
      println()

      if(x != 2)
        println("---|----|----")
    }
  }

  def stopGame(player: Player): Boolean = {
    val spots = player.getSpots()

    for(i <- 0 to 2) {
      if(spots.contains(List(i,0)) && spots.contains(List(i,1)) && spots.contains(List(i,2))) {
        println(player.getName() +  " is a winner !!")
        printBoardToConsole
        return true
      }
    }

    for(i <- 0 to 2) {
      if(spots.contains(List(0,i)) && spots.contains(List(1,i)) && spots.contains(List(2,i))) {
        println(player.getName() +  " is a winner !!")
        printBoardToConsole
        return true
      }
    }

    if(spots.contains(List(0,0)) && spots.contains(List(1,1)) && spots.contains(List(2,2))) {
      println(player.getName() +  " is a winner !!")
      printBoardToConsole
      return true
    }

    if(spots.contains(List(0,2)) && spots.contains(List(1,1)) && spots.contains(List(2,0))) {
      println(player.getName() +  " is a winner !!")
      printBoardToConsole
      return true
    }

    if(playerTwo.getSpots.length + playerOne.getSpots.length == 9){
      println("no winner !!")
      printBoardToConsole
      return true
    }

    false
  }

  def playerTurn() : List[Int] = {
    //def playerTurn(player: Player) : List[Int] = {
    def rowSpot(): Int = {
      var row = scala.io.StdIn.readInt()
      if (row < 3) row
      else {
        println("invalid spot, try again")
        rowSpot()
      }
    }

    def columnSpot() : Int = {
      var column = scala.io.StdIn.readInt()
      if (column < 3) {
        column
      }
      else {
        println("invalid spot, try again")
        columnSpot()
      }
    }

    var row = rowSpot()
    var col = columnSpot()

    if(emptySpot(List(row,col))){
      List(row,col)
    } else {
      println("spot taken, try again")
      playerTurn()
    }
  }

  def secandPlayer(player: Player): List[Int] = {
    if(player.isComputer) {
      computersTurn()
    }else {
      playerTurn()
    }
  }

  def computersTurn() : List[Int] = {
    var r = scala.util.Random
    var col = r.nextInt(3)
    var row = r.nextInt(3)

    if(emptySpot(List(row,col))){
      List(col, row)
    } else computersTurn
  }

  def introduction(): Unit = {

    def entername(player: Player): Unit = {
      println("please enter your name")
      val name = scala.io.StdIn.readLine()
      player.setName(name)
    }

    def pickNumOfPlayers(): Unit = {
      println("one player or two players, please enter 1 or 2")
      val numOfPlayers = scala.io.StdIn.readInt()

      numOfPlayers match  {
        case 1 => playerTwo.setIsComp(true)
        case 2 => (playerTwo.setIsComp(false), entername(playerTwo))
        case _ => pickNumOfPlayers
      }
    }

    entername(playerOne)
    pickNumOfPlayers

  }

  def playing() : Unit = {
    printBoardToConsole()
    println(playerOne.getName() + ", please go")
    playerOne.addSpot(playerTurn())

    if(!stopGame(playerOne)) {
      printBoardToConsole()
      println(playerTwo.getName() + ", please go")
      playerTwo.addSpot(secandPlayer(playerTwo))
      if(!stopGame(playerTwo)) {
        playing()
      }
    }
  }

  introduction()
  playing()
}

