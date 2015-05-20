#!/usr/bin/env runhaskell

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           System.Process

main :: IO ()
main = do
  output <- readProcess "ps" ["aux"] ""
  mapM_ putStrLn (extractPackageIds output)

extractPackageIds :: String -> [String]
extractPackageIds =
  lines >>>
  filter ("ghc" `isInfixOf`) >>>
  map (
    words >>>
    scanForPackageId) >>>
  catMaybes

scanForPackageId :: [String] -> Maybe String
scanForPackageId ("-package-id" : packageId : r) = Just packageId
scanForPackageId (a : r) = scanForPackageId r
scanForPackageId [] = Nothing
