package chess

import chess.models._

object BoardLogic {
  type Board = Array[Array[Char]]

  val PAWN_INITIAL_ROWS: List[Int] = List(1, 6)
  val PLAYS_SEPARATOR: String      = "#" * 29

  var gameBoard: Board =
    Array(
      Array('r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'),
      Array('p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'),
      Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
      Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
      Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
      Array(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
      Array('P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'),
      Array('R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R')
    )

  /**
    * Identifies a chess piece based on its character representation
    *
    * @return An `Option[ChessPiece]`, with the corresponding chess piece if the character is valid. None otherwise
    */
  def identifyChessPiece: Char => Option[ChessPiece] = { pieceRepr =>
    Option(pieceRepr match {
      case 'R' => Rook()
      case 'r' => Rook(false)
      case 'N' => Knight()
      case 'n' => Knight(false)
      case 'B' => Bishop()
      case 'b' => Bishop(false)
      case 'Q' => Queen()
      case 'q' => Queen(false)
      case 'K' => King()
      case 'k' => King(false)
      case 'P' => Pawn()
      case 'p' => Pawn(false)
      case _   => null
    })
  }

  /**
    * Checks if provided row / column positions are withing the chess board bounds
    *
    * @param row Row position
    * @param col Column position
    * @return Flag indicating if position is inside chess board bounds
    */
  def isWithinBounds(row: Int, col: Int): Boolean = row >= 0 && row <= 7 && col >= 0 && col <= 7

  /**
    * Prints the current state of the chess board to the console
    *
    * The chess board is displayed with row/column indexes for easy reference
    */
  def presentBoard(): Unit = {
    println("X  0  1  2  3  4  5  6  7")
    gameBoard.zipWithIndex.foreach {
      case (line, idx) => println(line.mkString(s"$idx [", "][", "]"))
    }
  }

}
