package chess.models

import chess.BoardLogic.Board
import chess.models.ChessPiece.advanceOne

final case class Queen(isLight: Boolean = true) extends StraightMovement with DiagonalMovement {
  def code: Char = if (isLight) 'Q' else 'q'

  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board] =
    (from, to) match {
      // Normal linear/diagonal movement
      case ((fColumn, fRow), (tColumn, tRow)) =>
        if (
          isMovementValid(currentBoard)(
            (advanceOne(fColumn, tColumn), advanceOne(fRow, tRow)),
            (tColumn, tRow)
          )
        )
          Right(updateBoard(currentBoard)(from, to))
        else Left("Queen path is blocked before reaching destination")

      case _ => Left("Invalid movement for Queen")
    }

}
