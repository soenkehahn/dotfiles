#!/usr/bin/env runhaskell

{-# LANGUAGE ViewPatterns #-}

import           Control.Applicative
import           Data.List
import           Data.Map as Map hiding (filter)
import           System.IO
import           System.Process

main :: IO ()
main = do
  args <- words <$> readProcess "kdialog" (words "--inputbox launcher") ""
  case args of
    [url] | isUrl url -> surf url
    [lookupBookmark -> Just url] -> surf url
    (lookupSearchEngine -> Just url) -> surf url
    _ -> surf ("https://www.google.com.sg/search?q=" ++ unwords args)

surf :: String -> IO ()
surf url = do
  hPutStrLn stderr ("opening " ++ url)
  callProcess "surf" [url]

isUrl :: String -> Bool
isUrl s = any (`isSuffixOf` s) $
  ".com" :
  ".org" :
  ".net" :
  ".de" :
  ".sg" :
  []

-- * bookmarks

bookmarks :: Bookmarks
bookmarks = Bookmarks $
  ("github", "github.com") :
  ("inbox", "inbox.google.com") :
  ("mail", "mail.google.com") :
  ("slack", "zalora.slack.com") :
  ("telegram", "web.telegram.org") :
  ("trello", "trello.com") :
  ("workflowy", "workflowy.com") :
  []

newtype Bookmarks = Bookmarks { getBookmarks :: [(String, String)] }

lookupBookmark :: String -> Maybe String
lookupBookmark arg =
  case filter (\ bookmark -> arg `isPrefixOf` fst bookmark) (getBookmarks bookmarks) of
    (bookmark : _) -> Just $ snd bookmark
    [] -> Nothing

-- * search engines

type SearchEngine = String -> String

lookupSearchEngine :: [String] -> Maybe String
lookupSearchEngine (bookmark : args) =
  case Map.lookup bookmark searchEngines of
    Nothing -> Nothing
    Just engine -> Just $ engine (unwords args)
lookupSearchEngine [] = Nothing

searchEngines :: Map String SearchEngine
searchEngines = fromList $
  ("we", \ query -> "https://en.wikipedia.org/w/index.php?search=" ++ query ++ "&title=Special%3ASearch&go=Go") :
  ("hack", \ query -> "https://hackage.haskell.org/packages/search?terms=" ++ query) :
  ("h", \ query -> "https://www.haskell.org/hoogle/?hoogle=" ++ query) :
  []
