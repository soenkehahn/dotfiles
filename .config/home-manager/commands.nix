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
]
