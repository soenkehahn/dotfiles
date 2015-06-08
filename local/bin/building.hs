#!/usr/bin/env runhaskell

import           Control.Arrow
import           Data.List
import           Data.Maybe
import           System.Process

main :: IO ()
main = do
  output <- readProcess "ps" ["aux"] ""
  let buildingPackages = case extractPackageNames output of
        [] -> "<nothing>"
        x -> unwords x
  putStrLn $ ("cabal is building: " ++ buildingPackages)

extractPackageNames :: String -> [String]
extractPackageNames =
  lines >>>
  filter ("ghc" `isInfixOf`) >>>
  map (
    words >>>
    scanForPackageName) >>>
  catMaybes >>>
  sort

scanForPackageName :: [String] -> Maybe String
scanForPackageName ("-package-name" : packageName : r) = Just packageName
scanForPackageName ("-this-package-key" : packageName : r) = Just packageName
scanForPackageName (a : r) = scanForPackageName r
scanForPackageName [] = Nothing
