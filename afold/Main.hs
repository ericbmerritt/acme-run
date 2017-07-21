{-# LANGUAGE OverloadedStrings#-}

module Main where

import qualified AcmeRun.Acme as Acme
import qualified AcmeRun.Lib as Lib
import Control.Shell
import qualified Data.Text as T

main :: IO ()
main =
  shell_ $ do
    winid <- Acme.winid
    Acme.filterSelected winid (run "fold" ["-w", "80", "-s"])
