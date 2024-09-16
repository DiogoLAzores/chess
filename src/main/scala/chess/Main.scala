package chess

import chess.BoardLogic._
import chess.models.King.isKingInCheck
import com.whitehatgaming.UserInputFile

import scala.annotation.tailrec

object Main extends App {
  private val movesFile = new UserInputFile(
    "C:\\Users\\Utilizador\\Documents\\ChessProj\\chess\\data\\sample-moves.txt"
  )

  /**
    * Recursively processes and simulates moves in the chess game
    *
    * This method handles each move, updates the chess board state, and checks for errors
    *
    * @param move An optional array of four Int representing the next move:
    *             - `fColumn`: '''F'''rom/starting column
    *             - `fRow`: '''F'''rom/starting row
    *             - `tColumn`: '''T'''arget column
    *             - `tRow`: '''T'''arget column
    * @param currentPlayerInCheck A flag indicating if the current player's king is in check
    * @param lastMoveResult The result of the last move:
    *                       - `Left(errorMessage)` if an error occurred, with the respective message
    *                       - `Right(true` if the last move was successful
    * @param lastMove An optional array representing the four integers that made the last move.
    *                 Used to check if the same player is attempting the current move
    * @param lastInvalidMove An optional array of four Int representing the last invalid move attempted.
    *                        Used to check if the same player is attempting the current move
    * @return This method prints the current chess board state, the result of the last move, and any errors encountered.
    *         It also handles player turns and checks if the current player's king is in check after each move
    */
  @tailrec
  private def play(
    move: Option[Array[Int]] = Option(movesFile.nextMove()),
    currentPlayerInCheck: Boolean = false
  )(
    lastMoveResult: Either[String, Boolean] = Right(true),
    lastMove: Option[Array[Int]] = None,
    lastInvalidMove: Option[Array[Int]] = None
  ): Unit = {
    // Output move's initial information
    println(PLAYS_SEPARATOR)
    lastMove.fold(println("Initial Board State"))(lMove =>
      println(s"Processing move: ${lMove.mkString("[", ", ", "]")}")
    )
    lastMoveResult.fold(error => println(s"[Error] $error"), _ => ())

    // Move processing
    move match {
      // No more moves to process
      case None =>
        // Since it is last move, inform that next player's king is in check
        if (currentPlayerInCheck) println("(Next player ends up IN CHECK)") else ()

        // Print last information to output
        presentBoard()
        println(PLAYS_SEPARATOR)
        println("#" * 7 + " No more moves " + "#" * 7)
        println(PLAYS_SEPARATOR)

      // Process new move
      case Some(Array(fColumn, fRow, tColumn, tRow)) =>
        val oldGameBoard      = gameBoard
        val currentChessPiece = gameBoard(fRow)(fColumn)
        val isCurrentLight    = currentChessPiece.isUpper

        // Inform if current player's king is in check, and output latest chess board state
        if (currentPlayerInCheck) println(s"[IN CHECK] Player ${if (isCurrentLight) "1" else "2"}")
        else ()
        presentBoard()

        // Check if the current player is the same as last/last invalid move's player
        val samePlayer = {
          // Helper partial function to check if the current play is equal to last/last invalid move's player
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
          // Acquire move outcome
          val outcome =
            // Check if movement exceeds board limits
            if (tColumn > gameBoard.length || tRow > gameBoard.length) {
              Left(s"Movement goes over board limits: $fColumn$fRow$tColumn$tRow")
            } else
              identifyChessPiece(gameBoard(fRow)(fColumn))
                .fold[Either[String, Board]](Left(s"No chess piece found in [$fColumn$fRow]"))(
                  _.move(gameBoard)((fColumn, fRow), (tColumn, tRow))
                )

          // Checks if current move was successful
          val (moveResult: Either[String, Boolean], currentKingInCheck) = outcome match {
            // Failed if own King is in check (board is reverted to not include movement)
            case Right(_) if isKingInCheck(isLight = isCurrentLight) =>
              gameBoard = oldGameBoard
              (Left("Player move failed since they are IN CHECK"), isKingInCheck(isLight = isCurrentLight))

            // Failed due to move error
            case error @ Left(_) => (error, currentPlayerInCheck)

            // Succeeded if own King is not in check
            case _ => (Right(true), isKingInCheck(isLight = !isCurrentLight))
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

  // Output initial game information
  println("Starting game. Player 1 has light/UPPERCASE pieces, Player 2 has dark/lowercase ones")
  println("('Castling', 'En Passant' and 'Promotion' moves aren't considered since not required by test)")
  play()()
}
