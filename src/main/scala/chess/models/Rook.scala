package chess.models

import chess.BoardLogic.Board
import chess.models.ChessPiece.advanceOne

final case class Rook(isLight: Boolean = true) extends ChessPiece {
  val name: String = "Rook"

  def code: String = if (isLight) "R" else "r"

  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board] =
    (from, to) match {
      // Normal linear movement, either traversing column or row
      case ((fColumn, fRow), (tColumn, tRow)) if fColumn == tColumn || fRow == tRow =>
        if (
          isMovementValid(currentBoard)(
            (advanceOne(fColumn, tColumn), advanceOne(fRow, tRow)),
            (tColumn, tRow)
          )
        )
          Right(updateBoard(currentBoard)(from, to))
        else Left("Rook path is blocked before reaching destination")

      // Invalid diagonal movement
      case _ => Left("Invalid non-linear movement for Rook")
    }
}
