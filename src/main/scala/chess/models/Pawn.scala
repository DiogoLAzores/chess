package chess.models

import chess.BoardLogic.{Board, PAWN_INITIAL_ROWS}
import chess.models.ChessPiece.advanceOne

final case class Pawn(isLight: Boolean = true) extends ChessPiece {
  val name: String = "Pawn"

  def code: String = if (isLight) "P" else "p"

  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board] =
    (from, to) match {
      // Invalid movement since the columns are different
      case ((fColumn, _), (tColumn, _)) if fColumn != tColumn =>
        Left(s"Column initial/final coordinates aren't the same")

      // Normal forward movement
      case ((fColumn, fRow), (tColumn, tRow)) if fColumn == tColumn =>
        val amountOfSpaces = Math.abs(fRow - tRow)

        // Check if 2-space movement is requested without it being the first Pawn move
        if (amountOfSpaces == 2 && !PAWN_INITIAL_ROWS.contains(fRow))
          Left("Pawn can't advance 2 spaces since it isn't its first movement")

        // Checks if movement is more than 1 space (after initial move)
        else if (amountOfSpaces > 2)
          Left("Pawn can't advance more than 1 space after initial move")

        // Checks if forward movement isn't blocked by chess piece (destination chess piece is ignored)
        else if (isMovementValid(currentBoard)((fColumn, advanceOne(fRow, tRow)), (fColumn, tRow)))
          Right(updateBoard(currentBoard)(from, to))
        else Left("Pawn path is blocked before reaching destination")

      // If pawn movement ends in a diagonal-forward advance
      case ((fColumn, fRow), (tColumn, tRow))
          if Math.abs(fRow - tRow) == 1 && (tColumn == fColumn - 1 || tColumn == fColumn + 1) =>
        // Checks if destination has a chess piece, fails movement if not
        if (currentBoard(tColumn)(tRow).isBlank)
          Left("Pawn can't go diagonally without a target in the destination")
        else Right(updateBoard(currentBoard)(from, to))

      case _ => Left("Invalid movement for Pawn")
    }
}
