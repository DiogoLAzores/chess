package chess.models

import chess.BoardLogic.{Board, gameBoard}

final case class King(isLight: Boolean = true) extends ChessPiece {
  def code: Char = if (isLight) 'K' else 'k'

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

  /**
    * Retrieves the position of the king on the chess board for the given side (light or dark)
    *
    * @return A tuple `(Int, Int)` representing the row/column indexes of the king on the board
    */
  private def getKing: Boolean => (Int, Int) = { isLight =>
    gameBoard.zipWithIndex.flatMap {
      case (row, rowIdx) =>
        row.zipWithIndex.collect {
          case (space, columnIdx) if space == (if (isLight) 'K' else 'k') =>
            (rowIdx, columnIdx)
        }
    }.head // Since we know 1 "K"/"k" will be found, it's safe to use ".head" here
  }

  /**
    * Calculates the new position based on the current king index (either row or column)
    *
    * @param kingPos Current king index (either row or column)
    * @param isUpper Flag indicating the direction of the increment/decrement
    * @return New row/column index which respects the chess board's boundaries
    */
  def calculateBound(kingPos: Int, isUpper: Boolean): Int = {
    if (isUpper) Math.min(kingPos + 1, 7) else Math.max(kingPos - 1, 0)
  }

  /**
    * Determine if the king of the specified side is in check by evaluating potential attacks from any of the opponent's
    * chess pieces
    *
    * @param isLight Flag indicating the side of the king to check
    * @return A flag indicating if the king in question is currently under check
    */
  def isKingInCheck(isLight: Boolean = true): Boolean = {
    val oppositePieces        = !isLight
    val (kingRow, kingColumn) = getKing(isLight)

    // Check if the king is attacked by an opponent's pawn
    val canPawnAttack = {
      val pawn           = Pawn(oppositePieces)
      val rowBound       = calculateBound(kingRow, isUpper = false)
      val posColumnBound = calculateBound(kingColumn, isUpper = true)
      val negColumnBound = calculateBound(kingColumn, isUpper = false)

      gameBoard(rowBound)(negColumnBound) == pawn.code || gameBoard(rowBound)(posColumnBound) == pawn.code
    }

    // Check if the king is attacked by the opponent's queen
    val canQueenAttack = {
      val queen = Queen(oppositePieces)
      queen.checkStraightAttacks(kingRow, kingColumn) ||
      queen.checkDiagonalAttacks(kingRow, kingColumn)
    }

    // Check if the king is attacked by an opponent's rook
    val canRookAttack = Rook(oppositePieces).checkStraightAttacks(kingRow, kingColumn)

    // Check if the king is attacked by an opponent's bishop
    val canBishopAttack = Bishop(oppositePieces).checkDiagonalAttacks(kingRow, kingColumn)

    // Check if the king is attacked by an opponent's knight
    val canKnightAttack = Knight(oppositePieces).checkAttacks(kingRow, kingColumn)

    // The king is in check if attacked by any opponent's piece
    canPawnAttack || canRookAttack || canBishopAttack || canQueenAttack || canKnightAttack
  }
}
