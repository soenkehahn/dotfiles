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
      "haskellScript"
      {
        buildInputs = [
          (haskellPackages.ghc.withPackages (p: [
            p.cradle
          ]))
        ];
      }
      ''
        ghc -threaded \
          -Wall -Werror \
          -Wno-name-shadowing \
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
]
