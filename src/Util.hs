{-# LANGUAGE NoImplicitPrelude #-}

module Util (ioFromMaybe) where

import Flow
import RIO
import System.IO.Error (userError)

ioFromMaybe :: Maybe a -> IO a
ioFromMaybe (Just a) = return a
ioFromMaybe _ = throwIO <| userError "empty"