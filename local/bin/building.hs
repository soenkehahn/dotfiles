#!/usr/bin/env runhaskell

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           System.Process

main :: IO ()
main = do
  output <- readProcess "ps" ["aux"] ""
  mapM_ putStrLn (extractPackageNames output)

extractPackageNames :: String -> [String]
extractPackageNames =
  lines >>>
  filter ("ghc" `isInfixOf`) >>>
  map (
    words >>>
    scanForPackageName) >>>
  catMaybes

scanForPackageName :: [String] -> Maybe String
scanForPackageName ("-package-name" : packageName : r) = Just packageName
scanForPackageName (a : r) = scanForPackageName r
scanForPackageName [] = Nothing
