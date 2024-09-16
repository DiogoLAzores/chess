package chess.models

import chess.BoardLogic.{gameBoard, isWithinBounds}

import scala.annotation.tailrec

trait DiagonalMovement extends ChessPiece

object DiagonalMovement {

  /**
    * Computes the new position on a grid based on the current row, column, and directional flags
    *
    * @param row The current row index of the piece
    * @param col The current column index of the piece
    * @param isPositive A flag indicating the direction of movement along the columns
    *                   - `true`: move right (toward higher column index)
    *                   - `false`: move left (toward lower column index)
    * @param isUpper A flag indicating the direction of movement along the rows
    *                - `true`: move upwards (toward higher row index)
    *                - `false`: move downwards (toward lower row index)
    * @return A tuple `(Int, Int)` representing the new row/column indexes after the move
    */
  private def move(row: Int, col: Int, isPositive: Boolean, isUpper: Boolean): (Int, Int) = {
    if (isPositive) {
      if (isUpper) (row + 1, col + 1) else (row - 1, col + 1)
    } else {
      if (isUpper) (row - 1, col - 1) else (row + 1, col - 1)
    }
  }

  /**
    * Recursively checks if the chess piece is found starting from a given position on the chess board and moving in a
    * specified direction
    *
    * @param chessPiece The chess piece to check for
    * @param row The starting row index on the board (starts with `kingRow`)
    * @param col The starting column index on the board (starts with `kingColumn`)
    * @param isPositive A flag indicating the horizontal movement direction
    *                   - `true`: move right (positive column direction)
    *                   - `false`: move left (negative column direction)
    * @param isUpper A flag indicating the vertical movement direction.
    *                - `true`: move upwards (positive row direction)
    *                - `false`: move downwards (negative row direction)
    * @return Flag indicating if the chess piece is found while diagonally traversing the chess board
    */
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

  /**
    * Implicit class that adds an extension method to chess pieces that are subtypes of `DiagonalMovement`
    *
    * @param chessPiece The chess piece instance being extended
    * @tparam CP Subtype of `DiagonalMovement`
    */
  implicit class DiagonalCheckOps[CP <: DiagonalMovement](chessPiece: ChessPiece) {

    /**
      * Checks if a chess piece poses as a threat, diagonally, to the opposite king
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
