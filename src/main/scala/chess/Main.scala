package chess

import chess.BoardLogic._
import chess.models.King.isKingInCheck
import com.whitehatgaming.UserInputFile

import scala.annotation.tailrec

object Main extends App {
  private val movesFile = new UserInputFile(
    "C:\\Users\\Utilizador\\Documents\\ChessProj\\chess\\data\\sample-moves.txt"
  )

  @tailrec
  private def play(
    move: Option[Array[Int]] = Option(movesFile.nextMove()),
    currentPlayerInCheck: Boolean = false
  )(
    lastMoveResult: Either[String, Boolean] = Right(true),
    lastMove: Option[Array[Int]] = None,
    lastInvalidMove: Option[Array[Int]] = None
  ): Unit = {
    println(PLAYS_SEPARATOR)
    lastMove.fold(println("Initial Board State"))(lMove =>
      println(s"Processing move: ${lMove.mkString("[", ", ", "]")}")
    )
    lastMoveResult.fold(error => println(s"[Error] $error"), _ => ())

    move match {
      // No more moves to process
      case None =>
        if (currentPlayerInCheck) println("(Next player ends up [IN CHECK])") else ()
        presentBoard()
        println(PLAYS_SEPARATOR)
        println("#" * 7 + " No more moves " + "#" * 7)
        println(PLAYS_SEPARATOR)

      // Process new move
      case Some(Array(fColumn, fRow, tColumn, tRow)) =>
        val oldGameBoard      = gameBoard
        val currentChessPiece = gameBoard(fRow)(fColumn)
        val isCurrentLight    = currentChessPiece.isUpper

        if (currentPlayerInCheck) println(s"[IN CHECK] Player ${if (isCurrentLight) "1" else "2"}")
        else ()
        presentBoard()

        // Check if the current player is the same as last/last invalid move's player
        val samePlayer = {
          val checkPlayer = (oldRow: Int, oldColumn: Int) => {
            val previousChessPiece = gameBoard(oldRow)(oldColumn)
            val isEqualUpper       = previousChessPiece.isUpper && currentChessPiece.isUpper
            val isEqualLower       = previousChessPiece.isLower && currentChessPiece.isLower

            isEqualUpper || isEqualLower
          }

          (lastMove, lastInvalidMove) match {
            // Player is the same as the last invalid move's player
            case (_, Some(Array(oldFColumn, oldFRow, _, _))) => checkPlayer(oldFRow, oldFColumn)
            // Player is the same as the last player
            case (Some(Array(_, _, oldTColumn, oldTRow)), _) => checkPlayer(oldTRow, oldTColumn)
            case _                                           => true
          }
        }

        // Play normally if either the last move was ok, or the same player is doing the current move
        if (lastMoveResult.contains(true) || samePlayer) {
          val outcome =
            // Check if movement exceeds board limits
            if (tColumn > gameBoard.length || tRow > gameBoard.length) {
              Left(s"Movement goes over board limits: $fColumn$fRow$tColumn$tRow")
            } else
              identifyChessPiece(gameBoard(fRow)(fColumn))
                .fold[Either[String, Board]](Left(s"No chess piece found in [$fColumn$fRow]"))(
                  _.move(gameBoard)((fColumn, fRow), (tColumn, tRow))
                )

          // Checks if current movement was successful
          val (moveResult: Either[String, Boolean], currentKingInCheck) = outcome match {
            // Failed due to move error
            case error @ Left(_) =>
              (error, currentPlayerInCheck)

            // Failed if own King is in check (board is reverted to not include movement)
            case Right(_) if isKingInCheck(isLight = isCurrentLight) =>
              val ownKingInCheckError = "Player move failed since they are [IN CHECK]"
              gameBoard = oldGameBoard
              (Left(ownKingInCheckError), isKingInCheck(isLight = isCurrentLight))

            // Succeeded if own King is not in check
            case _ =>
              (Right(true), isKingInCheck(isLight = !isCurrentLight))
          }

          // Send current outcome's result with the current move to next move's logic, moving to the next player
          play(currentPlayerInCheck = currentKingInCheck)(
            moveResult,
            lastMove = move,
            lastInvalidMove = Option.when(moveResult.isLeft)(move).flatten
          )
        } else {
          // Send invalid move and player error with last move to next move's logic, maintaining the player
          play(currentPlayerInCheck = isKingInCheck(isLight = isCurrentLight))(
            Left("Invalid move, it's not your turn"),
            lastMove = move,
            lastInvalidMove = lastInvalidMove
          )
        }

      case Some(_) => ()
    }
  }

  println("Starting game. Player 1 has light/UPPERCASE pieces, Player 2 has dark/lowercase ones")
  println("('Castling', 'En Passant' and 'Promotion' moves aren't considered since not required by test)")
  play()()
}
