package chess.models

import chess.BoardLogic.gameBoard

import scala.annotation.tailrec

trait DiagonalMovement extends ChessPiece

object DiagonalMovement {
  private def isWithinBounds(row: Int, col: Int): Boolean = row >= 0 && row <= 7 && col >= 0 && col <= 7

  private def move(row: Int, col: Int, isPositive: Boolean, isUpper: Boolean): (Int, Int) = {
    if (isPositive) {
      if (isUpper) (row + 1, col + 1) else (row - 1, col + 1)
    } else {
      if (isUpper) (row - 1, col - 1) else (row + 1, col - 1)
    }
  }

  @tailrec
  private def checkFrom(
    chessPiece: ChessPiece
  )(row: Int, col: Int, isPositive: Boolean, isUpper: Boolean): Boolean = {
    if (isWithinBounds(row, col)) {
      if (gameBoard(row)(col) == chessPiece.code) true
      else {
        val (nextRow, nextCol) = move(row, col, isPositive, isUpper)
        checkFrom(chessPiece)(nextRow, nextCol, isPositive, isUpper)
      }
    } else false
  }

  implicit class DiagonalCheckOps[CP <: DiagonalMovement](chessPiece: ChessPiece) {

    /**
      * Checks if a chess piece can attack the opposite king diagonally
      *
      * @param kingRow King row position
      * @param kingColumn King column position
      * @return Flag indicating if any attacks are possible
      */
    def checkDiagonalAttacks(kingRow: Int, kingColumn: Int): Boolean = {
      val posUpDiagonal   = checkFrom(chessPiece)(kingRow, kingColumn, isPositive = true, isUpper = true)
      val posDownDiagonal = checkFrom(chessPiece)(kingRow, kingColumn, isPositive = true, isUpper = false)
      val negUpDiagonal   = checkFrom(chessPiece)(kingRow, kingColumn, isPositive = false, isUpper = true)
      val negDownDiagonal = checkFrom(chessPiece)(kingRow, kingColumn, isPositive = false, isUpper = false)

      posUpDiagonal || posDownDiagonal || negUpDiagonal || negDownDiagonal
    }
  }
}
