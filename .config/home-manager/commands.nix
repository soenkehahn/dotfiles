{ system, pkgs, inputs }:
let
  haskellScript =
    let
      haskellPackages = pkgs.haskellPackages.override {
        overrides = final: prev: {
          cradle = (inputs.cradle.lib.${system}.mkCradle final);
        };
      };
    in
    { name, text }: pkgs.runCommand
      name
      {
        buildInputs = [
          (haskellPackages.ghc.withPackages (p: [
            p.cradle
            p.getopt-generics
            p.temporary
          ]))
        ];
        meta.mainProgram = name;
      }
      ''
        ghc -threaded \
          -Wall -Werror \
          -Wno-name-shadowing \
          -XViewPatterns \
          ${pkgs.writeText "Main.hs" text} \
          -o ${name}
        mkdir -p $out/bin
        cp ./${name} $out/bin/
      '';
in
[
  (pkgs.writeShellApplication {
    name = "git-gone";
    text = ''
      git fetch -p
      git branch -vv | rg ': gone\]' | choose 0
      git branch -vv | rg ': gone\]' | choose 0 | parallel -j1 'git branch -D'
    '';
  })
  (pkgs.writeShellApplication {
    name = "mirror-screen";
    runtimeInputs = [ pkgs.wf-recorder ];
    text = ''
      export SDL_VIDEODRIVER=wayland
      wf-recorder -c rawvideo -m sdl -f pipe:wayland-mirror --output eDP-1
    '';
  })
  (haskellScript {
    name = "gm";
    text = ''
      import Cradle
      import System.Environment

      main :: IO ()
      main = do
        args <- getArgs
        case args of
          [] -> run $ cmd "git" &
            addArgs ["commit", "-v"]
          args -> run $ cmd "git" &
            addArgs ["commit", "--message", unwords args]
    '';
  })
  (haskellScript {
    name = "cleanup-nix-store-roots";
    text = ''
      import Control.Monad
      import Cradle
      import Data.List
      import Data.String.Conversions
      import System.Directory
      import System.Exit
      import System.FilePath
      import System.IO

      gcRootsDir :: FilePath
      gcRootsDir = "/nix/var/nix/gcroots/auto"

      main :: IO ()
      main = do
        links <- listDirectory gcRootsDir
        forM_ links $ \ link -> do
          StdoutTrimmed (cs -> root) <- run $ cmd "readlink" & addArgs [(gcRootsDir </> link)]
          when ("/home/shahn" `isPrefixOf` root) $ do
            putStr $ "Delete " <> root <> "? [y/n/q] "
            hFlush stdout
            reply <- getLine
            when (reply == "q") $ do
              exitWith ExitSuccess
            when (reply == "y") $ do
              removeFile root
              putStrLn $ "Deleted " <> root
    '';
  })
  (pkgs.writeScriptBin
    "git-watch-tree"
    ''
      if git rev-parse --show-toplevel
      then
        cd $(git rev-parse --show-toplevel)

        tput rmam

        while true ; do
          export lines=$(tput lines)
          find .git/ | rg -v '^.git/objects/' | \
            entr -rcds 'git -c color.ui=always log --oneline --graph --decorate --remotes --branches | head -n $(expr $lines - 1)'
        done
      else
        while true ; do
          sleep 100
        done
      fi
    '')
  (haskellScript {
    name = "temp-checkout";
    text = ''
      import Control.Monad
      import Cradle
      import System.Directory
      import System.FilePath
      import System.IO.Temp
      import WithCli

      main :: IO ()
      main = withCli $ \ (repo :: String, pr :: Maybe Int) -> do
        tempDir <- do
          parent <- getCanonicalTemporaryDirectory
          createTempDirectory parent "temp-checkout"
        run_ $ cmd "git"
          & addArgs ["clone", "git@github.com:" <> repo]
          & setWorkingDir tempDir
        [repo] <- listDirectory tempDir
        let repoDir = tempDir </> repo
        forM_ pr $ \ pr -> do
          run_ $ cmd "gh"
            & addArgs ["pr", "checkout", show pr]
            & setWorkingDir repoDir
        run_ $ cmd "zsh"
          & setWorkingDir (tempDir </> repo)
    '';
  })
]
