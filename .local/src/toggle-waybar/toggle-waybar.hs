#!/usr/bin/env stack
{-
stack script
  --resolver lts-18.16 --install-ghc
  --compile
  --package getopt-generics
  --package shake
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Monad
import Development.Shake
import WithCli

data Args =
  Args
  deriving (Generic)

instance HasArguments Args

main :: IO ()
main =
  withCli $ \(_ :: Args) -> do
    running <- isRunning "waybar"
    case running of
      [] -> unit $ cmd Shell "waybar"
      _ -> forM_ running $ \pid -> unit $ cmd "kill -15" (show pid)

isRunning :: String -> IO [Integer]
isRunning needle = do
  Stdout (output :: String) <- cmd "ps aux"
  return $
    map fst $
    filter (\(_pid, process) -> process == needle) $
    map (\line -> (read (words line !! 1), words line !! 10)) $
    drop 1 $ lines output
