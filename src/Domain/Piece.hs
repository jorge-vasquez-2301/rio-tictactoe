{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.Piece
  ( Piece (..),
    next,
    make,
  )
where

import RIO
import RIO.Text (toUpper)

data Piece = X | O deriving (Eq, Show)

next :: Piece -> Piece
next X = O
next O = X

make :: Text -> Maybe Piece
make value =
  case toUpper value of
    "X" -> Just X
    "O" -> Just O
    _ -> Nothing