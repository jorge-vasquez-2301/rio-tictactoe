{-# LANGUAGE NoImplicitPrelude #-}

module Domain.Player
  ( Player (..),
  )
where

import RIO

data Player = Computer | Human deriving (Eq)