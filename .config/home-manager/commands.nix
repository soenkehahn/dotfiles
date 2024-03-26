{ pkgs }: [
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
