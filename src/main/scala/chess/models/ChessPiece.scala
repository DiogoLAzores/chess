package chess.models

import chess.BoardLogic.Board
import chess.models.ChessPiece.advanceOne

import scala.annotation.tailrec

trait ChessPiece {

  /** Chess piece side */
  val isLight: Boolean

  /** How the chess piece is represented in the chess board, conditioned by the associated player's color */
  def code: Char

  /**
    * Attempts to move the piece from an initial position to the target position, given its specific traversal features
    *
    * @param from Initial position tuple, with the row and column indexes, respectively
    * @param to Final position tuple, with the row and column indexes, respectively
    * @return An Either returning a string if a problem occurred, or the updated chess board if move was successful
    */
  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board]

  /**
    * Updates the board with the successful move
    *
    * @param currentBoard Current board state
    * @param from Initial position to clear
    * @param to Final position to update with chess piece
    * @return Updated board
    */
  final def updateBoard(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Board = {
    (from, to) match {
      case ((fColumn, fRow), (tColumn, tRow)) =>
        currentBoard(fRow).update(fColumn, ' ')
        currentBoard(tRow).update(tColumn, code)
        currentBoard
    }
  }

  /**
    * Traverses a chess piece through the board to reach the limits, while checking if any middle spaces are empty
    * (enabling traversal if so)
    *
    * @param currentBoard Current board state
    * @param currentPos Current column/row position to be checked. '''Start in the next expected position'''
    * @param targets Column/Row limits to reach
    * @return Boolean indicating if movement is successful/without chess pieces blocking the way
    * @note When starting, consider the ''currentPos'' to be the next space where the chess piece would go after its
    *       initial position
    */
  @tailrec
  final def isMovementValid(currentBoard: Board)(currentPos: (Int, Int), targets: (Int, Int)): Boolean =
    (currentPos, targets) match {
      // The final position was reached successfully
      case ((cColumn, cRow), (columnTarget, rowTarget)) if cColumn == columnTarget && cRow == rowTarget =>
        true

      // The next position isn't blocked by a chess piece
      case ((cColumn, cRow), (columnTarget, rowTarget)) if currentBoard(cRow)(cColumn) == ' ' =>
        // Reached column target, increments row coordinate
        if (cColumn == columnTarget)
          isMovementValid(currentBoard)((cColumn, advanceOne(cRow, rowTarget)), targets)

        // Reached row target, increments column coordinate
        else if (cRow == rowTarget)
          isMovementValid(currentBoard)((advanceOne(cColumn, columnTarget), cRow), targets)

        // Didn't reach target, both coordinates are incremented
        else
          isMovementValid(currentBoard)(
            (advanceOne(cColumn, columnTarget), advanceOne(cRow, rowTarget)),
            targets
          )

      // Remaining cases are considered as invalid movements
      case _ => false
    }
}

object ChessPiece {

  /**
    * Based on current and target coordinate, check if it's a positive or negative movement
    *
    * @param current Current coordinate
    * @param target Target coordinate
    * @return Next movement coordinate
    */
  def advanceOne(current: Int, target: Int): Int =
    if (current > target) current - 1
    else if (current < target) current + 1
    else current
}
