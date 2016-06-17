package ticktocktoe

/**
  * Created by Maier Lasdun on 6/6/2016.
  */


object Main extends App {


  var playerOneSpots: List[List[Int]] = List()
  var playerTwoSpots: List[List[Int]] = List()


  def emptySpot(attempt :List[Int]) : Boolean = {
    !(playerOneSpots.contains(attempt) ||  playerTwoSpots.contains(attempt))
  }

  def printToConsole(): Unit = {
    def OwnsSpot(l: List[Int]): String = {
      if(playerOneSpots.contains(l))
        "X"
      else if(playerTwoSpots.contains(l))
        "O"
      else " "
    }

    def makeLine(i: Int): String = {
      if(i != 2) " | " else ""
    }

    for(x <- 0 to 2) {
      for(y <- 0 to 2) {
        var line: String = if(y != 2) " | " else ""

        print(" " + OwnsSpot(List(x, y)) + line)
      }
      println()

      if(x != 2)
        println("---|----|----")
    }
  }

  def stopGame(player: List[List[Int]]): Boolean = {
    for(i <- 0 to 2) {
      if(player.contains(List(i,0)) && player.contains(List(i,1)) && player.contains(List(i,2))) {
        println("winner !!")
        printToConsole
        return true
      }
    }

    for(i <- 0 to 2) {
      if(player.contains(List(0,i)) && player.contains(List(1,i)) && player.contains(List(2,i))) {
        println("winner !!")
        printToConsole
        return true
      }
    }

    if(player.contains(List(0,0)) && player.contains(List(1,1)) && player.contains(List(2,2))) {
      println("winner !!")
      printToConsole
      return true
    }

    if(player.contains(List(0,2)) && player.contains(List(1,1)) && player.contains(List(2,0))) {
      println("winner !!")
      printToConsole
      return true
    }

    if(playerTwoSpots.length + playerOneSpots.length == 9){
      println("no winner !!")
      printToConsole
      return true
    }

    false
  }

  def playerTurn() : List[Int] = {

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
      playerOneSpots = playerOneSpots :+ List(row, col)
      List(row,col)
    } else {
      println("spot taken, try again")
      playerTurn
    }
  }



  def computersTurn() : List[Int] = {
    var r = scala.util.Random
    var col = r.nextInt(3)
    var row = r.nextInt(3)

    if(emptySpot(List(row,col))){
      playerTwoSpots = playerTwoSpots :+ List(row, col)
      List(col, row)
    } else computersTurn
  }

  def playing() : Unit = {
    printToConsole()
    playerTurn()
    if(!stopGame(playerOneSpots)) {
      computersTurn()
      if(!stopGame(playerTwoSpots)) {
        playing()
      }
    }
  }


  playing()
}


//temp printing spots
/*for(i <- playerOneSpots) {
  print("player one " + i(0) + ", " + i(1) + ".  ")
}
println()*/
// end of printing


//temp printing spots
/*for(i <- playerTwoSpots) {
  print("player two " + i(0) + ", " + i(1)+ ".  ")
}
println() */
// end of printing
