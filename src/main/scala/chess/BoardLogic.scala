package chess

import chess.models._

object BoardLogic {
  type Board = Array[Array[String]]

  val LIGHT_INITIAL_ROWS: List[Int] = List(6, 7)
  val DARK_INITIAL_ROWS: List[Int]  = List(0, 1)
  val PAWN_INITIAL_ROWS: List[Int]  = List(1, 6)

  var gameBoard: Board =
    Array(
      Array("r", "n", "b", "q", "k", "b", "n", "r"),
      Array("p", "p", "p", "p", "p", "p", "p", "p"),
      Array(" ", " ", " ", " ", " ", " ", " ", " "),
      Array(" ", " ", " ", " ", " ", " ", " ", " "),
      Array(" ", " ", " ", " ", " ", " ", " ", " "),
      Array(" ", " ", " ", " ", " ", " ", " ", " "),
      Array("P", "P", "P", "P", "P", "P", "P", "P"),
      Array("R", "N", "B", "Q", "K", "B", "N", "R")
    )

  def identifyChessPiece: String => Option[ChessPiece] = { pieceRepr =>
    val piece = pieceRepr match {
      case "R" => Rook()
      case "r" => Rook(false)
      case "N" => Knight()
      case "n" => Knight(false)
      case "B" => Bishop()
      case "b" => Bishop(false)
      case "Q" => Queen()
      case "q" => Queen(false)
      case "K" => King()
      case "k" => King(false)
      case "P" => Pawn()
      case "p" => Pawn(false)
      case _   => null
    }

    Option(piece)
  }

  def presentBoard(): Unit = {
    println("X  0  1  2  3  4  5  6  7")
    gameBoard.zipWithIndex.foreach {
      case (line, idx) => println(line.mkString(s"$idx [", "][", "]"))
    }
  }

}
