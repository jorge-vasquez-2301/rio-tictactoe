{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Applicative
import Data.List.Split (chunksOf)
import Data.Text.IO (getLine, putStrLn)
import Domain.Board (Board)
import qualified Domain.Board as Board
import Domain.Field
import qualified Domain.Field as Field
import Domain.GameResult (GameResult)
import qualified Domain.GameResult as GameResult
import Domain.Piece (Piece)
import qualified Domain.Piece as Piece
import qualified Domain.Player as Player
import Domain.State (OngoingState (..), OverState (..), State (..))
import qualified Domain.State as State
import Flow
import RIO
import RIO.Set (isSubsetOf)
import qualified RIO.Set as Set
import RIO.Text (intercalate)
import qualified RIO.Text as Text
import RIO.Vector ((!?))
import System.Random
import Util

choosePlayerPiece :: IO Piece
choosePlayerPiece = do
  input <- putStrLn "Do you want to be X or O?: " *> getLine
  (ioFromMaybe <| Piece.make input) <|> (putStrLn "Invalid input" *> choosePlayerPiece)

whichPieceGoesFirst :: IO Piece
whichPieceGoesFirst =
  fmap
    ( \case
        True -> Piece.X
        False -> Piece.O
    )
    (randomIO :: IO Bool)

programLoop :: State -> IO ()
programLoop (Ongoing state@OngoingState {..}) = drawBoard currentBoard *> (step state >>= programLoop)
programLoop (Over OverState {..}) = drawBoard board

drawBoard :: Board -> IO ()
drawBoard board =
  Field.allFields
    |> Set.toList
    |> map (\field -> (Board.getField field board, fromEnum field))
    |> map
      ( \case
          (Just piece, _) -> show piece
          (Nothing, value) -> show value
      )
    |> map Text.pack
    |> chunksOf 3
    |> map (\fields -> " " <> intercalate " ║ " fields <> " ")
    |> intercalate "\n═══╬═══╬═══\n"
    |> putStrLn

step :: OngoingState -> IO State
step state@OngoingState {..} = do
  nextMove <- if State.isComputerTurn <| State.ongoing state then getComputerMove currentBoard else getPlayerMove currentBoard
  takeField state nextMove

getComputerMove :: Board -> IO Field
getComputerMove board = do
  let size = length <| Board.unoccupiedFields board
  index <- fmap (`mod` size) (randomIO :: IO Int)
  _ <- putStrLn "Waiting for computer's move, press a key to continue..." *> getLine
  ioFromMaybe (Board.unoccupiedFields board !? index)

getPlayerMove :: Board -> IO Field
getPlayerMove board = do
  input <- putStrLn "What's your next move? (0-8): " *> getLine
  tmpField <- (ioFromMaybe <| Field.make input) <|> (putStrLn "Invalid input" *> getPlayerMove board)
  if Board.fieldIsNotFree tmpField board
    then putStrLn "That field has been already used!" *> getPlayerMove board
    else return tmpField

takeField :: OngoingState -> Field -> IO State
takeField state@OngoingState {..} field = do
  let updatedBoard = Board.updated field turn currentBoard
  let updatedTurn = Piece.next turn
  let gameResult = getGameResult updatedBoard
  case gameResult of
    Just gameResult -> (putStrLn <| GameResult.show gameResult) *> return (State.Over <| OverState updatedBoard)
    Nothing -> return <| State.ongoing state {currentBoard = updatedBoard, turn = updatedTurn}

getGameResult :: Board -> Maybe GameResult
getGameResult board =
  let crossWin = isWinner board Piece.X
      noughtWin = isWinner board Piece.O
   in case (crossWin, noughtWin) of
        (True, True) -> error "It should not be possible for both players to win!"
        (True, _) -> Just <| GameResult.Win Piece.X
        (_, True) -> Just <| GameResult.Win Piece.O
        _ -> Nothing

isWinner :: Board -> Piece -> Bool
isWinner board piece =
  let occupiedFields = Set.fromList <| Board.fieldsOccupiedByPiece piece board
   in any (\combinations -> Set.fromList combinations `isSubsetOf` occupiedFields) Board.winnerCombinations

main :: IO ()
main = do
  playerPiece <- choosePlayerPiece
  pieceThatGoesFirst <- whichPieceGoesFirst
  putStrLn ((Text.pack <| show pieceThatGoesFirst) <> " goes first")
  let initialState = OngoingState {currentBoard = Board.empty, whoIsCross = if playerPiece == Piece.X then Player.Human else Player.Computer, turn = pieceThatGoesFirst}
  programLoop <| State.ongoing initialState
