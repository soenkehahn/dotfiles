#!/usr/bin/env stack
{- stack runghc --resolver lts-10.0
  --package shake
  --package getopt-generics
  --package logging-facade
-}
{-# LANGUAGE DeriveGeneric #-}

import Development.Shake
import qualified System.Logging.Facade as Log
import WithCli
import Data.Char

data Args = Args
  { host :: String
  } deriving (Generic)

instance HasArguments Args

main :: IO ()
main = do
  withCli $ \(Args targetHost) -> do
    sourceHost <- getHostName
    mapM_ (syncDir targetHost) (dirs sourceHost targetHost)
    return () :: IO ()

getHostName :: IO String
getHostName = do
  Stdout output <- cmd "hostname"
  return $ trim output

dirs :: String -> String -> [FilePath]
dirs source target =
  ["important", "passwords"] ++
  if any excludeMusic [source, target]
    then []
    else ["musik/beets"]
  where
    excludeMusic "taipei" = True
    excludeMusic _ = False

syncDir :: String -> FilePath -> IO ()
syncDir host path = do
  Log.info ("syncing '" ++ path ++ "' ...")
  unit $
    cmd
      Shell
      ("unison-gtk ssh://" ++
       host ++ "//home/shahn/" ++ path ++ "/ ~/" ++ path ++ "/")

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
