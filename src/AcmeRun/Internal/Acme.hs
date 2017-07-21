{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# LANGUAGE NamedFieldPuns #-}

module AcmeRun.Internal.Acme where

import qualified Control.Foldl as Fold
import qualified Data.List as List
import qualified Data.Text as T
import Prelude hiding (FilePath, read)
import Turtle

type WinId = T.Text

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
  , builtIns :: [T.Text]
  , userTags :: [T.Text]
  } deriving (Show)

--
-- $setup
--
-- >>> :set -XOverloadedStrings
elementFromString :: T.Text -> Maybe Element
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

elementToString :: Element -> T.Text
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

winid :: Shell (Maybe WinId)
winid = liftIO $ need "winid"

acmePath :: WinId -> Element -> T.Text
acmePath lWinId element =
  T.concat ["acme", "/", lWinId, "/", elementToString element]

read :: WinId -> Element -> Shell Line
read lWinId element = inproc "9p" ["read", acmePath lWinId element] empty

write :: WinId -> Element -> Shell Line -> Shell Line
write lWinId element = inproc "9p" ["write", acmePath lWinId element]

cleanTags :: [T.Text] -> [T.Text]
cleanTags = List.filter (/= "") . List.map T.strip

parseBuiltins :: T.Text -> Tag
parseBuiltins potentialBuiltIns =
  let tags = T.splitOn " " potentialBuiltIns
  in if T.isPrefixOf "/" potentialBuiltIns
       then case tags of
              fname:builtIns ->
                Tag
                { fileName = Just $ fromText fname
                , builtIns = cleanTags builtIns
                , userTags = []
                }
              builtIns -> Tag {fileName = Nothing, builtIns, userTags = []}
       else Tag {fileName = Nothing, builtIns = cleanTags tags, userTags = []}

-- | My function description
-- 
-- >>> parseTag "/home/user/workspace/acme-run/-personal Del Snarf | Look  Send"
-- Tag {fileName = Just (FilePath "/home/user/workspace/acme-run/-personal"), builtIns = ["Del","Snarf"], userTags = ["Look","Send"]}
--
-- >>> parseTag "New Cut Paste Snarf Sort Zerox Delcol"
-- Tag {fileName = Nothing, builtIns = ["New","Cut","Paste","Snarf","Sort","Zerox","Delcol"], userTags = []} 
-- 
-- >>> parseTag "/home/user/workspace/acme-run/-personal Del Snarf | Look  Send Del Snarf | aindent"
-- Tag {fileName = Just (FilePath "/home/user/workspace/acme-run/-personal"), builtIns = ["Del","Snarf"], userTags = ["Look","Send","Del","Snarf","aindent"]}
parseTag :: T.Text -> Tag
parseTag lTag =
  case T.splitOn "|" lTag of
    bis:userTags ->
      (parseBuiltins bis)
      {userTags = cleanTags (T.splitOn " " $ T.concat userTags)}
    [] -> Tag {fileName = Nothing, builtIns = [], userTags = []}

tag :: WinId -> Shell Tag
tag lWinId = (parseTag . linesToText) <$> fold (read lWinId Tag_) Fold.list

body :: WinId -> Shell Line
body lWinId = read lWinId Body

selected :: WinId -> Shell Line
selected lWinId = read lWinId RdSel

writeSelected :: WinId -> Shell Line -> Shell Line
writeSelected lWinId = write lWinId WrSel

writeBody :: WinId -> Shell Line -> Shell Line
writeBody lWinId = write lWinId Data

filterBody :: WinId -> (Shell Line -> Shell Line) -> Shell ()
filterBody lWinId fltr = do
  _ <- write lWinId Addr (return "1,$")
  _ <- writeBody lWinId (fltr $ body lWinId)
  return ()

filterSelected :: WinId -> (Shell Line -> Shell Line) -> Shell ()
filterSelected lWinId fltr = do
  _ <- writeSelected lWinId (fltr $ selected lWinId)
  return ()
