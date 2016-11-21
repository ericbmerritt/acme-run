{-# LANGUAGE OverloadedStrings, LambdaCase, ScopedTypeVariables #-}

module Main where

import qualified AcmeRun.Acme as Acme
import qualified AcmeRun.Lib as Lib
import qualified AcmeRun.Sh as Sh
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Filesystem.Path.CurrentOS (encodeString)
import Prelude hiding (FilePath)
import Turtle

scalaConfigArgs :: FilePath -> Shell [T.Text]
scalaConfigArgs checkPath =
  Lib.findDominatingFile [((), ".scalafmt.conf")] checkPath >>= \case
    Just (_, dir) ->
      return ["--config", T.pack (encodeString $ dir </> ".scalafmt.conf")]
    Nothing -> return []

formatScala :: FilePath -> Shell (Shell Line -> Shell Line)
formatScala checkPath = do
  args <- scalaConfigArgs checkPath
  return (inproc "scalafmt" ("--stdin":args))

main :: IO ()
main =
  sh $
  Acme.winid >>= \case
    Just winid ->
      (Acme.fileName <$> Acme.tag winid) >>= \case
        Just winPath ->
          case extension winPath of
            Just ".hs" ->
              Acme.filterBody
                winid
                (inproc
                   "hindent"
                   ["--style", "gibiansky", "--line-length", "80"])
            Just ".scala" -> do
              fmtScala <- formatScala winPath
              Acme.filterBody winid fmtScala
            Just ext -> do
              liftIO
                (TextIO.putStrLn $ format ("don't know how to format: " %s) ext)
              exit $ ExitFailure 1
            Nothing -> do
              liftIO $ echo "No extension provided on Window filename"
              exit $ ExitFailure 1
        Nothing -> do
          liftIO $ echo "Window has no file name"
          exit $ ExitFailure 1
    Nothing -> do
      liftIO $ echo "Unable to find $winid"
      exit $ ExitFailure 1
