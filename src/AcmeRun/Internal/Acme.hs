{-# LANGUAGE NamedFieldPuns #-}

module AcmeRun.Internal.Acme where

import Control.Shell
import qualified Data.List as List
import Data.List.Split (wordsBy)
import Data.String.Utils (startswith)
import Prelude hiding (read)

type WinId = String

data Element
  = Addr
  | Body
  | Ctl
  | Data
  | EditOut
  | Errors
  | Event
  | RdSel
  | Tag_
  | WrSel
  | Xdata
  deriving (Show)

data Tag = Tag
  { fileName :: Maybe FilePath
  , builtIns :: [String]
  , userTags :: [String]
  } deriving (Show)

--
-- $setup
--
-- >>> :set -XOverloadedStrings
elementFromString :: String -> Maybe Element
elementFromString "addr" = Just Addr
elementFromString "body" = Just Body
elementFromString "ctl" = Just Ctl
elementFromString "data" = Just Data
elementFromString "editout" = Just EditOut
elementFromString "errors" = Just Errors
elementFromString "event" = Just Event
elementFromString "rdsel" = Just RdSel
elementFromString "tag" = Just Tag_
elementFromString "wrsel" = Just WrSel
elementFromString "xdata" = Just Xdata
elementFromString _ = Nothing

elementToString :: Element -> String
elementToString Addr = "addr"
elementToString Body = "body"
elementToString Ctl = "ctl"
elementToString Data = "data"
elementToString EditOut = "editout"
elementToString Errors = "errors"
elementToString Event = "event"
elementToString RdSel = "rdsel"
elementToString Tag_ = "tag"
elementToString WrSel = "wrsel"
elementToString Xdata = "xdata"

winid :: Shell WinId
winid = getEnv "winid"

acmePath :: WinId -> Element -> String
acmePath lWinId element =
  List.concat ["acme", "/", lWinId, "/", elementToString element]

read :: WinId -> Element -> Shell ()
read lWinId element = run "9p" ["read", acmePath lWinId element]

read_ :: WinId -> Element -> Shell String
read_ lWinId element = capture $ read lWinId element

write :: WinId -> Element -> Shell ()
write lWinId element = run "9p" ["write", acmePath lWinId element]

parseBuiltins :: String -> Tag
parseBuiltins potentialBuiltIns =
  let tags = wordsBy (== ' ') potentialBuiltIns
  in if startswith "/" potentialBuiltIns
       then case tags of
              fname:builtIns ->
                Tag {fileName = Just fname, builtIns = builtIns, userTags = []}
              builtIns -> Tag {fileName = Nothing, builtIns, userTags = []}
       else Tag {fileName = Nothing, builtIns = tags, userTags = []}

-- | My function description
-- 
-- >>> parseTag "/home/user/workspace/acme-run/-personal Del Snarf | Look  Send"
-- Tag {fileName = Just "/home/user/workspace/acme-run/-personal", builtIns = ["Del","Snarf"], userTags = ["Look","Send"]}
--
-- >>> parseTag "New Cut Paste Snarf Sort Zerox Delcol"
-- Tag {fileName = Nothing, builtIns = ["New","Cut","Paste","Snarf","Sort","Zerox","Delcol"], userTags = []} 
-- 
-- >>> parseTag "/home/user/workspace/acme-run/-personal Del Snarf | Look  Send Del Snarf | aindent"
-- Tag {fileName = Just "/home/user/workspace/acme-run/-personal", builtIns = ["Del","Snarf"], userTags = ["Look","Send","Del","Snarf","aindent"]}
parseTag :: String -> Tag
parseTag lTag =
  case wordsBy (== '|') lTag of
    bis:userTags ->
      (parseBuiltins bis) {userTags = wordsBy (== ' ') $ concat userTags}
    [] -> Tag {fileName = Nothing, builtIns = [], userTags = []}

tag :: WinId -> Shell Tag
tag lWinId = parseTag <$> read_ lWinId Tag_

body :: WinId -> Shell String
body lWinId = read_ lWinId Body

selected :: WinId -> Shell String
selected lWinId = read_ lWinId RdSel

filterBody :: WinId -> Shell () -> Shell ()
filterBody lWinId fltr = do
  echo_ "1,$" |> write lWinId Addr
  (read lWinId Body |> fltr) |> write lWinId Data

filterSelected :: WinId -> Shell () -> Shell ()
filterSelected lWinId fltr = (read lWinId RdSel |> fltr) |> write lWinId WrSel
