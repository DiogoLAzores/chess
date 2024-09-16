package chess.models

import chess.BoardLogic.gameBoard
import chess.models.ChessPiece.advanceOne
import chess.models.King.calculateBound

import scala.annotation.tailrec

trait StraightMovement extends ChessPiece

object StraightMovement {

  /**
    * Checks if a chess piece can attack king horizontally/vertically
    *
    * @param currentIdx Current index being checked
    * @param maxIdx Limit to where the current index can go
    * @param wasThreat Flag that indicates if the chess piece, in the previous position, was a thread to the king
    * @param isHorizontal Flag that indicates if the check is being checked horizontally or not
    * @return Flag telling if the chess piece can attack king
    */
  @tailrec
  private def checkStraight(chessPiece: ChessPiece, kingColumn: Int, kingRow: Int)(
    currentIdx: Int,
    maxIdx: Int,
    isHorizontal: Boolean,
    wasThreat: Boolean = false
  ): Boolean = {
    val squareChessPiece =
      if (isHorizontal) gameBoard(kingRow)(currentIdx) else gameBoard(currentIdx)(kingColumn)

    if (wasThreat) true
    if (squareChessPiece != " ") squareChessPiece == chessPiece.code
    else if (squareChessPiece == chessPiece.code) true
    else
      checkStraight(chessPiece, kingColumn, kingRow)(
        advanceOne(currentIdx, maxIdx),
        maxIdx,
        wasThreat,
        isHorizontal
      )
  }

  private def checkStraightDirection(
    chessPiece: ChessPiece
  )(kingColumn: Int, kingRow: Int, isHorizontal: Boolean): Boolean = {
    val boundCheck    = calculateBound(if (isHorizontal) kingColumn else kingRow, isUpper = true)
    val negBoundCheck = calculateBound(if (isHorizontal) kingColumn else kingRow, isUpper = false)

    checkStraight(chessPiece, kingColumn, kingRow)(boundCheck, 7, isHorizontal) ||
    checkStraight(chessPiece, kingColumn, kingRow)(negBoundCheck, 0, isHorizontal)
  }

  implicit class StraightCheckOps[CP <: StraightMovement](chessPiece: ChessPiece) {

    /**
      * Checks if a chess piece can attack the opposite king either horizontally or vertically
      *
      * @param kingRow King row position
      * @param kingColumn King column position
      * @return Flag indicating if any attacks are possible
      */
    def checkStraightAttacks(kingRow: Int, kingColumn: Int): Boolean =
      checkStraightDirection(chessPiece)(kingColumn, kingRow, isHorizontal = true) || // Horizontal Check
        checkStraightDirection(chessPiece)(kingColumn, kingRow, isHorizontal = false) // Vertical Check
  }
}
