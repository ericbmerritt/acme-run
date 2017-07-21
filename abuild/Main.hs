{-# LANGUAGE LambdaCase #-}

module Main where

import qualified AcmeRun.Acme as Acme
import qualified AcmeRun.Lib as Lib
import Control.Shell hiding (ExitReason)
import System.Exit (exitFailure)

data Types
  = Makefile
  | Sbt
  | Stack
  deriving (Show)

--
-- These are in order of priority. So the first thing
-- it finds in this list is the thing that it will build
--
dominating :: [Lib.Dominating Types]
dominating =
  [ (Sbt, "build.sbt")
  , (Stack, "stack.yaml")
  , (Makefile, "makefile")
  , (Makefile, "Makefile")
  ]

build :: FilePath -> Shell ()
build filename =
  Lib.findDominatingFile dominating filename >>= \case
    Just (Makefile, dir) -> inDirectory dir $ run "make" []
    Just (Stack, dir) -> inDirectory dir $ run "stack" ["build"]
    Just (Sbt, dir) -> inDirectory dir $ run "sbt" ["compile"]
    Nothing -> do
      echo "Unable to find build file"
      liftIO exitFailure

main :: IO ()
main =
  shell_ $ do
    winid <- Acme.winid
    Acme.fileName <$> Acme.tag winid >>= \case
      Just filename -> build filename
      Nothing -> do
        echo "Window has no filename"
        liftIO exitFailure
