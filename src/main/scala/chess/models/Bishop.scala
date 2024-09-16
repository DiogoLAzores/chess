package chess.models

import chess.BoardLogic.Board
import chess.models.ChessPiece.advanceOne

final case class Bishop(isLight: Boolean = true) extends DiagonalMovement {
  val name: String = "Bishop"

  def code: String = if (isLight) "B" else "b"

  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board] =
    (from, to) match {
      case ((fColumn, fRow), (tColumn, tRow)) if fColumn == tColumn || fRow == tRow =>
        Left("Bishop can't move go straight")
      // Normal diagonal movement
      case ((fColumn, fRow), (tColumn, tRow)) =>
        if (
          isMovementValid(currentBoard)(
            (advanceOne(fColumn, tColumn), advanceOne(fRow, tRow)),
            (tColumn, tRow)
          )
        )
          Right(updateBoard(currentBoard)(from, to))
        else Left("Bishop path is blocked before reaching destination")
    }

}
