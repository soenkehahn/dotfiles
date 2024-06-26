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
      , cargo.simple "cargo-edit"
      , cargo.simple "cargo-expand"
      , cargo.simple "cargo-limit"
      , cargo.simple "cargo-watch"
      , cargo.simple "exa"
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
      ]
    }
