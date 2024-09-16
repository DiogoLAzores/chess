package chess

import chess.models._

object BoardLogic {
  type Board = Array[Array[Char]]

  val LIGHT_INITIAL_ROWS: List[Int] = List(6, 7)
  val DARK_INITIAL_ROWS: List[Int]  = List(0, 1)
  val PAWN_INITIAL_ROWS: List[Int]  = List(1, 6)

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

  def presentBoard(): Unit = {
    println("X  0  1  2  3  4  5  6  7")
    gameBoard.zipWithIndex.foreach {
      case (line, idx) => println(line.mkString(s"$idx [", "][", "]"))
    }
  }

}
