{-# LANGUAGE OverloadedStrings #-}

module AcmeRun.Internal.Lib where

import qualified Control.Foldl as Fold
import Data.List (foldl)
import Filesystem.Path (filename, parent)
import Prelude hiding (FilePath)
import Turtle

type Dominating a = (a, FilePath)

getDominatingType ::
     FilePath
  -> Dominating a
  -> Maybe (Dominating a)
  -> FilePath
  -> Maybe (Dominating a)
getDominatingType _parentDir _dominating el@(Just _) _child = el
getDominatingType parentDir (fileType, dominating) Nothing child =
  let base = filename child
  in if dominating == base
       then Just (fileType, parentDir)
       else Nothing

searchDirectoryListing ::
     (Show a)
  => FilePath
  -> [FilePath]
  -> Maybe (Dominating a)
  -> Dominating a
  -> Maybe (Dominating a)
searchDirectoryListing _parentDir _children el@(Just _) _dominatingPair = el
searchDirectoryListing parentDir children Nothing dominatingPair =
  foldl (getDominatingType parentDir dominatingPair) Nothing children

searchDirectoryByDominating ::
     (Show a)
  => FilePath
  -> [Dominating a]
  -> [FilePath]
  -> Maybe (Dominating a)
searchDirectoryByDominating parentDir dominating children =
  foldl (searchDirectoryListing parentDir children) Nothing dominating

findDominatingFile ::
     (Show a) => [Dominating a] -> FilePath -> Shell (Maybe (a, FilePath))
findDominatingFile _ "/" = return Nothing
findDominatingFile dominating currentPath =
  let parentDirectory = parent currentPath
  in do children <- fold (ls parentDirectory) Fold.list
        case searchDirectoryByDominating parentDirectory dominating children of
          Nothing -> findDominatingFile dominating parentDirectory
          el@(Just _) -> return el
