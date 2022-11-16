{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.GameResult
  ( GameResult (..),
    show,
  )
where

import Domain.Piece (Piece (O, X))
import RIO hiding (show)

data GameResult = Win Piece | Draw

show :: GameResult -> Text
show (Win X) = "Cross wins!"
show (Win O) = "Nought wins!"
show Draw = "It's a draw!"