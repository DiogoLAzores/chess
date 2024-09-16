package chess.models

import chess.BoardLogic.{gameBoard, isWithinBounds}

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
  private def checkFor(
    chessPiece: ChessPiece
  )(row: Int, col: Int, isPositive: Boolean, isUpper: Boolean): Boolean = {
    if (isWithinBounds(row, col)) {
      val squarePiece = gameBoard(row)(col)

      if (squarePiece == chessPiece.code) true
      else {
        val (nextRow, nextCol) = move(row, col, isPositive, isUpper)
        checkFor(chessPiece)(nextRow, nextCol, isPositive, isUpper)
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
      val posUpDiagonal   = checkFor(chessPiece)(kingRow, kingColumn, isPositive = true, isUpper = true)
      val posDownDiagonal = checkFor(chessPiece)(kingRow, kingColumn, isPositive = true, isUpper = false)
      val negUpDiagonal   = checkFor(chessPiece)(kingRow, kingColumn, isPositive = false, isUpper = true)
      val negDownDiagonal = checkFor(chessPiece)(kingRow, kingColumn, isPositive = false, isUpper = false)

      posUpDiagonal || posDownDiagonal || negUpDiagonal || negDownDiagonal
    }
  }
}
