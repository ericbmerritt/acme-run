{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import qualified AcmeRun.Acme as Acme
import qualified AcmeRun.Lib as Lib
import qualified AcmeRun.Sh as Sh
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Prelude hiding (FilePath)
import Turtle

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

doBuild :: Shell () -> FilePath -> Shell ()
doBuild cmd dir = do
  liftIO $ TextIO.putStrLn (format ("Building in directory " %fp) dir)
  Sh.chdir dir cmd

build :: FilePath -> Shell ()
build filename =
  Lib.findDominatingFile dominating filename >>= \case
    Just (Makefile, dir) -> doBuild (stdout (inproc "make" [] empty)) dir
    Just (Stack, dir) -> doBuild (stdout (inproc "stack" ["build"] empty)) dir
    Just (Sbt, dir) -> doBuild (stdout (inproc "sbt" ["compile"] empty)) dir
    Nothing -> do
      liftIO $ echo "Unable to find build file"
      exit $ ExitFailure 1

main :: IO ()
main =
  sh $
  Acme.winid >>= \case
    Just winid ->
      Acme.fileName <$> Acme.tag winid >>= \case
        Just filename -> build filename
        Nothing -> do
          liftIO $ echo "Window has no filename"
          exit $ ExitFailure 1
    Nothing -> do
      liftIO $ echo "Unable to get $winid"
      exit $ ExitFailure 1
