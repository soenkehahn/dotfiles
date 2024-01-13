let Package = { name : Text, skip : List Text, install : Text }

let bash = ./bash.dhall

let cargo = ./cargo.dhall

let stack = ./stack.dhall

let simple = ./simple.dhall

in  { packages =
      [ simple
          "peck-test-executable"
          ''
          mkdir -p ~/.local/bin/
          echo "echo huhu" > ~/.local/bin/peck-test-executable
          chmod +x ~/.local/bin/peck-test-executable
          ''
      , simple
          "sl"
          ''
          git clone https://github.com/mtoyoda/sl.git
          cd sl
          make
          mkdir -p ~/.local/bin/
          cp sl ~/.local/bin/sl
          ''
      , { name = "el"
        , skip = [ "~/.stack" ]
        , install =
            bash
              ''
              git clone https://github.com/soenkehahn/el
              cd el
              stack install
              ''
        }
      , stack "lts-19.30" "markdown-unlit"
      , simple
          "rustup-init"
          ''
          curl -O https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
          chmod +x rustup-init
          mkdir -p ~/.local/bin/
          mv rustup-init ~/.local/bin/
          ''
      , { name = "neovim-remote"
        , skip = [ "~/.cache" ]
        , install =
            bash
              ''
              pip3 install --user neovim-remote
              ''
        }
      , cargo.simple "alacritty"
      , cargo.simple "cargo-edit"
      , cargo.simple "cargo-expand"
      , cargo.simple "cargo-limit"
      , cargo.simple "cargo-watch"
      , cargo.simple "exa"
      , cargo.simple "flavours"
      , cargo.simple "mdbook"
      , cargo.simple "mdbook-linkcheck"
      , cargo.simple "rust-script"
      , cargo.simple "sfz"
      , cargo.simple "tracetree"
      , cargo.fromGithub
          "imsnif"
          "bandwhich"
          "45503a01a687208cdc61be3fda25b1603d008653"
      , cargo.fromGithub "soenkehahn" "si" "master"
      , let version = "1.85.1"

        in  simple
              "vscode-${version}"
              ''
              curl 'https://update.code.visualstudio.com/${version}/linux-x64/stable' -Lo VSCode-linux-x64.tar.gz
              tar --no-same-owner -xf VSCode-linux-x64.tar.gz
              cp -r VSCode-linux-x64 ~/.local/opt/
              ln -s ../opt/VSCode-linux-x64/bin/code ~/.local/bin/code
              ''
      , simple
          "ipfs"
          ''
          curl -LO https://github.com/ipfs/kubo/releases/download/v0.17.0/kubo_v0.17.0_linux-amd64.tar.gz
          tar --no-same-owner -xvf kubo_v0.17.0_linux-amd64.tar.gz
          cp kubo/ipfs ~/.local/bin/
          ''
      , { name = "toggle-waybar"
        , skip = [ "~/.stack" ]
        , install =
            bash
              ''
              cp -v ~/.local/src/toggle-waybar/* .
              stack --resolver lts-18.18 ghc toggle-waybar.hs
              cp -v toggle-waybar ~/.local/bin/
              ''
        }
      , let version = "12.5.6"

        in  simple
              "tor-browser"
              ''
              curl 'https://dist.torproject.org/torbrowser/${version}/tor-browser-linux64-${version}_ALL.tar.xz' -LO
              tar --no-same-owner -xf tor-browser-linux64-${version}_ALL.tar.xz
              cp -r tor-browser ~/.local/opt/
              echo 'cd ~/.local/opt/tor-browser ; ./start-tor-browser.desktop "$@"' > ~/.local/bin/tor-browser
              chmod +x ~/.local/bin/tor-browser
              ''
      ]
    }
