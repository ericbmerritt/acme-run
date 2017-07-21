{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import qualified AcmeRun.Acme as Acme
import qualified AcmeRun.Lib as Lib
import Control.Shell
import Prelude hiding (FilePath)
import System.Exit (exitFailure)

scalaConfigArgs :: FilePath -> Shell [String]
scalaConfigArgs checkPath =
  Lib.findDominatingFile [((), ".scalafmt.conf")] checkPath >>= \case
    Just (_, dir) -> return ["--config", dir </> ".scalafmt.conf"]
    Nothing -> return []

formatScala :: FilePath -> Shell ()
formatScala checkPath = do
  confArgs <- scalaConfigArgs checkPath
  run "scalafmt" ("--stdin" : confArgs)

main :: IO ()
main =
  shell_ $ do
    winid <- Acme.winid
    Acme.fileName <$> Acme.tag winid >>= \case
      Just winPath ->
        case takeExtension winPath of
          ".hs" ->
            Acme.filterBody
              winid
              (run "hindent" ["--style", "gibiansky", "--line-length", "80"])
          ".scala" -> Acme.filterBody winid $ formatScala winPath
          ext -> do
            echo $ "don't know how to format: " ++ ext
            liftIO exitFailure
      Nothing -> do
        echo "Window has no file name"
        liftIO exitFailure
