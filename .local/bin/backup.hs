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

data Args = Args
  { host :: String
  } deriving (Generic)

instance HasArguments Args

main :: IO ()
main =
  withCli $ \(Args host) -> do
    mapM_ (syncDir host) (dirs host)
    return () :: IO ()

dirs :: String -> [FilePath]
dirs host =
  ["important", "passwords"] ++
  case host of
    "taipei" -> []
    _ -> ["musik/beets"]

syncDir :: String -> FilePath -> IO ()
syncDir host path = do
  Log.info ("syncing '" ++ path ++ "' ...")
  unit $
    cmd
      Shell
      ("unison-gtk ssh://" ++
       host ++ "//home/shahn/" ++ path ++ "/ ~/" ++ path ++ "/")
