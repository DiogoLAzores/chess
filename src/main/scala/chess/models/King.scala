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
  def calculateBound(kingPos: Int, isUpper: Boolean): Int = {
    if (isUpper) Math.min(kingPos + 1, 7) else Math.max(kingPos - 1, 0)
  }

  def getKing(isLight: Boolean = true): (Int, Int) = {
    gameBoard.zipWithIndex.flatMap {
      case (row, rowIdx) =>
        row.zipWithIndex.collect {
          case (space, columnIdx) if space == (if (isLight) "K" else "k") =>
            (rowIdx, columnIdx)
        }
    }.head // Since we know 1 "K"/"k" will be found, it's safe to use ".head" here
  }

  def isKingInCheck(isLight: Boolean = true): Boolean = {
    val oppositePieces        = !isLight
    val (kingRow, kingColumn) = getKing(isLight)

    val canPawnAttack = {
      val pawn           = Pawn(oppositePieces)
      val rowBound       = calculateBound(kingRow, isUpper = false)
      val posColumnBound = calculateBound(kingColumn, isUpper = true)
      val negColumnBound = calculateBound(kingColumn, isUpper = false)

      val diagonalLeftRight = gameBoard(rowBound)(negColumnBound) == pawn.code
      val diagonalRightLeft = gameBoard(rowBound)(posColumnBound) == pawn.code

      diagonalLeftRight || diagonalRightLeft
    }

    val canRookAttack = {
      val rook = Rook(oppositePieces)
      rook.checkStraightAttacks(kingRow, kingColumn)
    }

    val canBishopAttack = {
      val bishop = Bishop(oppositePieces)
      bishop.checkDiagonalAttacks(kingRow, kingColumn)
    }

    val canQueenAttack = {
      val queen = Queen(oppositePieces)
      queen.checkStraightAttacks(kingRow, kingColumn) ||
      queen.checkDiagonalAttacks(kingRow, kingColumn)
    }

    val canKnightAttack = {
      val knight = Knight(oppositePieces)
      knight.checkAttacks(kingRow, kingColumn)
    }

    canPawnAttack || canRookAttack || canBishopAttack || canQueenAttack || canKnightAttack
  }
}
