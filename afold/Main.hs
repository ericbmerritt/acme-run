{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import qualified AcmeRun.Acme as Acme
import qualified AcmeRun.Sh as Sh
import qualified AcmeRun.Lib as Lib
import qualified Data.Text as T
import Turtle
  
main :: IO ()
main =
  sh $ 
    Acme.winid >>= \case
      Just winid -> Acme.filterSelected winid (inproc "fold" ["-w", "80", "-s"])
      Nothing -> do
        liftIO $ echo "Unable to find $winid"
        exit $ ExitFailure 1
  