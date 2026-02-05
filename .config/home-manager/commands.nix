{ system, inputs, pkgs, lib, jail }:
let
  haskellScript =
    let
      haskellPackages = pkgs.haskellPackages.override {
        overrides = final: prev: {
          cradle = (inputs.cradle.lib.${system}.mkCradle final);
        };
      };
    in
    { name, runtimeInputs ? [ ], text }: pkgs.runCommand
      name
      {
        buildInputs = [
          (haskellPackages.ghc.withPackages (p: [
            p.cradle
            p.getopt-generics
            p.temporary
          ]))
        ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
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
        wrapProgram "$out/bin/${name}" \
          --prefix PATH : ${pkgs.lib.makeBinPath runtimeInputs}
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
  (pkgs.writeShellApplication {
    name = "pick-colortheme";
    text =
      let
        script = pkgs.lib.getExe (pkgs.writeShellApplication
          {
            name = "script";
            text = ''
              set-colortheme "$(set-colortheme list | fzf --ansi | choose -1)"
              press any key...
              read -r
            '';
          });
      in
      ''
        alacritty --command ${script}
      '';
  })
  (
    pkgs.writeShellApplication {
      name = "signal";
      runtimeInputs = [ pkgs.signal-desktop ];
      text = ''
        signal-desktop --no-sandbox
      '';
    }
  )
  (haskellScript {
    name = "iso-date";
    text = ''
      import Cradle
      import WithCli

      main :: IO ()
      main = withCli $ do
        pure () :: IO ()
        run_ $ cmd "date"
          & addArgs ["--rfc-3339=seconds"]
    '';
  })
  (
    pkgs.writeShellApplication {
      name = "shahn-startup";
      text = ''
        if ! test -e /run/opengl-driver ; then
          echo linking ${pkgs.mesa.drivers} to /run/opengl-driver
          sudo ln -s ${pkgs.mesa.drivers} /run/opengl-driver
        fi
      '';
    }
  )
  (
    pkgs.writeShellApplication {
      name = "switch-colortheme";
      text = ''
        ${./switch-colortheme}
      '';
    }
  )
  (haskellScript {
    name = "clementine";
    runtimeInputs = [ pkgs.clementine ];
    text = ''
      import Cradle
      import System.Environment

      main :: IO ()
      main = do
        args <- getArgs
        run_ $
          cmd "clementine"
            & addArgs args
            & modifyEnvVar "QT_SCALE_FACTOR" (const $ Just "2")
    '';
  })
  (haskellScript {
    name = "prag";
    text = ''
      import Cradle

      main :: IO ()
      main = do
        run_ $
          cmd "ssh"
            & addArgs [
              "prag.goat-noodlefish.ts.net",
              "-t",
              "zellij",
              "attach",
              "-c",
              "gary"
            ]
    '';
  })
  (
    let
      f = pkgs.firefox-esr.override {
        extraPolicies = {
          ExtensionSettings = {
            "uBlock0@raymondhill.net" = {
              installation_mode = "force_installed";
              install_url = "file://${inputs.ublock}";
            };
            "ext@alexdav.id" = {
              installation_mode = "force_installed";
              install_url = "file://${inputs.alex-ff-ext.packages.${system}.extension}";
            };
          };
        };
        # nativeMessagingHosts = [alex-ff-ext.native-messaging];
      };
      cfg =
        {
          "xpinstall.signatures.required" = false; # Disable checking signature
          "extensions.autoDisableScopes" = 14; # Allow use of extensions without user intervention
          "privacy.clearOnShutdown.cookies" = false;
          "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
        };
      userJs = pkgs.lib.trivial.pipe cfg [
        (pkgs.lib.attrsets.mapAttrsToList (k: v: "user_pref(${builtins.toJSON k},${builtins.toJSON v});"))
        (builtins.concatStringsSep "\n")
        (pkgs.writeText "user.js")
      ];
    in
    pkgs.writeScriptBin "firefox" ''
      #! ${pkgs.lib.getExe ((import ./nushell.nix {inherit pkgs lib;}).nushell)}

      let firefox_dir = $"($env.HOME)/.mozilla/firefox"

      # Find the profile section where Default=1
      let default_profile = open $"($firefox_dir)/profiles.ini"
        | transpose key value
        | where { $in.key | str starts-with "Profile" }
        | where { $in.value.Default? == "1" }
        | first
        | get value.Path
      let profile = $"($firefox_dir)/($default_profile)"

      cp -f ${userJs} $"($profile)/user.js"
      ${pkgs.lib.getExe f}
    ''
  )
]
