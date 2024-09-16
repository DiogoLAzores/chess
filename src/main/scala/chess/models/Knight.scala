package chess.models

import chess.BoardLogic.{Board, gameBoard, isWithinBounds}

final case class Knight(isLight: Boolean = true) extends ChessPiece {
  def code: Char = if (isLight) 'N' else 'n'

  def move(currentBoard: Board)(from: (Int, Int), to: (Int, Int)): Either[String, Board] =
    (from, to) match {
      /*
        Normal movement to special Knight position:
          - When columns have a 2-square difference, rows' difference should be of 1-square
          - When columns have a 1-square difference, rows' difference should be of 2-squares
       */
      case ((fColumn, fRow), (tColumn, tRow))
          if (Math.abs(fColumn - tColumn) == 2 && Math.abs(fRow - tRow) == 1) ||
            (Math.abs(fColumn - tColumn) == 1 && Math.abs(fRow - tRow) == 2) =>
        Right(updateBoard(currentBoard)(from, to))

      case _ => Left("Invalid movement for Knight")
    }
}

object Knight {
  implicit class KingInCheck(knight: Knight) {
    def checkAttacks(kingRow: Int, kingColumn: Int): Boolean = {
      val knightMoves = List(
        (-2, -1),
        (-2, 1),
        (-1, -2),
        (-1, 2),
        (1, -2),
        (1, 2),
        (2, -1),
        (2, 1)
      )

      knightMoves.exists {
        case (rowOffset, colOffset) =>
          val newRow    = kingRow + rowOffset
          val newColumn = kingColumn + colOffset
          isWithinBounds(newRow, newColumn) && gameBoard(newRow)(newColumn) == knight.code
      }
    }
  }
}
