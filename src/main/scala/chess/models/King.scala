package chess.models

import chess.BoardLogic.{Board, gameBoard}

final case class King(isLight: Boolean = true) extends ChessPiece {
  val name: String = "King"

  def code: String = if (isLight) "K" else "k"

  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board] =
    (from, to) match {
      // Normal movement, with +1 movement in either the row or the column
      case ((fColumn, fRow), (tColumn, tRow))
          if Math.abs(fColumn - tColumn) == 1 || Math.abs(fRow - tRow) == 1 =>
        Right(updateBoard(currentBoard)(from, to))
      case _ => Left("Invalid movement for King")
    }
}

object King {
  def getKing(isLight: Boolean = true): (Int, Int) = {
    gameBoard.zipWithIndex.flatMap {
      case (row, rowIdx) =>
        row.zipWithIndex.collect {
          case (space, columnIdx) if space == (if (isLight) "K" else "k") =>
            (rowIdx, columnIdx)
        }
    }.head // Since we know 1 "K"/"k" will be found, it's safe to use ".head" here
  }
}
