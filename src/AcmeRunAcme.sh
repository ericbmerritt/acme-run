{-# LANGUAGE OverloadedStrings, LambdaCase, ExtendedDefaultRules
  #-}

module AcmeRun.Acme (WinId, Element, elementFromString, elementToString
					, winid, acmePath, tag_, tag, fileName_, fileName) where

import AcmeRun.Internal.Acme