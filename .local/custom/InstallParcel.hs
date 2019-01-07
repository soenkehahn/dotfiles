#!/usr/bin/env stack
{- stack runghc
--resolver lts-13.1 --install-ghc
--package shake
-}

import Development.Shake
import System.Directory
import System.FilePath

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  shakeArgs shakeOptions $ do
    let parcelExecutable = homeDir </> ".local/bin/parcel"
    want [parcelExecutable]

    parcelExecutable %> \ _ -> do
      cmd "yarn global add parcel-bundler --prefix"
        (homeDir </> ".local")
