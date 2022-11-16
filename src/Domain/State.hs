{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.State
  ( State (..),
    OngoingState (..),
    OverState (..),
    isComputerTurn,
    ongoing,
  )
where

import Domain.Board (Board)
import Domain.Piece (Piece)
import qualified Domain.Piece as Piece
import Domain.Player (Player)
import qualified Domain.Player as Player
import RIO

data State = Ongoing OngoingState | Over OverState

data OngoingState = OngoingState {currentBoard :: Board, whoIsCross :: Player, turn :: Piece}

newtype OverState = OverState {board :: Board}

isComputerTurn :: State -> Bool
isComputerTurn (Ongoing (OngoingState {..})) =
  (turn == Piece.X && whoIsCross == Player.Computer) || (turn == Piece.O && whoIsCross == Player.Human)
isComputerTurn (Over _) = False

ongoing :: OngoingState -> State
ongoing state = Ongoing state