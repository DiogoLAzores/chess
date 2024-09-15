package chess.models

import chess.BoardLogic.{Board, gameBoard}

final case class Knight(isLight: Boolean = true) extends ChessPiece {
  val name: String = "Knight"

  def code: String = if (isLight) "N" else "n"

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
    }
}

object Knight {
  implicit class KingInCheck(knight: Knight) {
    def checkAttacks(kingRow: Int, kingColumn: Int): Boolean = {
      val validPlusRows: Int => Boolean     = kingRow + _ <= 7
      val validMinusRows: Int => Boolean    = kingRow - _ >= 0
      val validPlusColumns: Int => Boolean  = kingColumn + _ <= 7
      val validMinusColumns: Int => Boolean = kingColumn - _ >= 0

      val pos1 =
        if (validMinusRows(1) && validMinusColumns(2)) gameBoard(kingRow - 1)(kingColumn - 2) else ""
      val pos2 =
        if (validMinusRows(2) && validMinusColumns(1)) gameBoard(kingRow - 2)(kingColumn - 1) else ""
      val pos3 =
        if (validPlusRows(1) && validMinusColumns(2)) gameBoard(kingRow + 1)(kingColumn - 2) else ""
      val pos4 =
        if (validPlusRows(2) && validMinusColumns(1)) gameBoard(kingRow + 2)(kingColumn - 1) else ""
      val pos5 =
        if (validMinusRows(2) && validPlusColumns(1)) gameBoard(kingRow - 2)(kingColumn + 1) else ""
      val pos6 =
        if (validMinusRows(1) && validPlusColumns(2)) gameBoard(kingRow - 1)(kingColumn + 2) else ""
      val pos7 =
        if (validPlusRows(1) && validPlusColumns(2)) gameBoard(kingRow + 1)(kingColumn + 2) else ""
      val pos8 =
        if (validPlusRows(2) && validPlusColumns(1)) gameBoard(kingRow + 2)(kingColumn + 1) else ""

      List(pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8).contains(knight.code)
    }
  }
}
