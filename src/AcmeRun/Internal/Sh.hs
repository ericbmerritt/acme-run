{-# LANGUAGE OverloadedStrings #-}

module AcmeRun.Internal.Sh where

import Control.Exception (bracket)
import Filesystem.Path.CurrentOS (encodeString)
import qualified GHC.IO
import Prelude hiding (FilePath)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import Turtle

directorySetup :: FilePath -> IO GHC.IO.FilePath
directorySetup filePath = do
  originalDir <- getCurrentDirectory
  setCurrentDirectory $ encodeString filePath
  return originalDir

chdir :: FilePath -> Shell () -> Shell ()
chdir path action =
  liftIO $ bracket (directorySetup path) setCurrentDirectory (const $ sh action)
