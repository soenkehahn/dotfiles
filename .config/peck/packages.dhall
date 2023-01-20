let bash = ./bash.dhall

let cargo = ./cargo.dhall

let def = { skip = [] : List Text }

let Package = { name : Text, skip : List Text, install : Text }

let stack =
      \(resolver : Text) ->
      \(name : Text) ->
        { name
        , skip = [ "~/.stack" ]
        , install =
            bash
              ''
              stack install --resolver=${resolver} ${name}
              ''
        }

in  { packages =
      [     def
        //  { name = "peck-test-executable"
            , install =
                bash
                  ''
                  mkdir -p ~/.local/bin/
                  echo "echo huhu" > ~/.local/bin/peck-test-executable
                  chmod +x ~/.local/bin/peck-test-executable
                  ''
            }
      ,     def
        //  { name = "sl"
            , install =
                bash
                  ''
                  git clone https://github.com/mtoyoda/sl.git
                  cd sl
                  make
                  mkdir -p ~/.local/bin/
                  cp sl ~/.local/bin/sl
                  ''
            }
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
      , stack "lts-19.30" "dhall-lsp-server"
      , stack "lts-19.30" "dhall"
      , stack "lts-19.30" "hpack"
      , stack "lts-19.30" "markdown-unlit"
      ,     def
        //  { name = "rustup-init"
            , install =
                bash
                  ''
                  curl -O https://static.rust-lang.org/rustup/dist/x86_64-unknown-linux-gnu/rustup-init
                  chmod +x rustup-init
                  mkdir -p ~/.local/bin/
                  mv rustup-init ~/.local/bin/
                  ''
            }
      ,     def
        //  { name = "as-tree"
            , skip = [ "~/.wget-hsts" ]
            , install =
                bash
                  ''
                  wget 'https://github.com/jez/as-tree/releases/download/0.12.0/as-tree-0.12.0-linux.zip'
                  aunpack as-tree-0.12.0-linux.zip
                  cp as-tree ~/.local/bin/as-tree
                  ''
            }
      ,     def
        //  { name = "yq"
            , skip = [ "~/.cache" ]
            , install =
                bash
                  ''
                  pip3 install --user yq
                  ''
            }
      ,     def
        //  { name = "neovim-remote"
            , skip = [ "~/.cache" ]
            , install =
                bash
                  ''
                  pip3 install --user neovim-remote
                  ''
            }
      , cargo.simple "alacritty"
      , cargo.fromGithub
          "imsnif"
          "bandwhich"
          "45503a01a687208cdc61be3fda25b1603d008653"
      , cargo.simple "bottom"
      , cargo.simple "cargo-edit"
      , cargo.simple "cargo-expand"
      , cargo.simple "choose"
      , cargo.simple "du-dust"
      , cargo.simple "exa"
      , cargo.simple "fd-find"
      , cargo.simple "just"
      , cargo.simple "rust-script"
      , cargo.simple "sd"
      , cargo.simple "starship"
      , cargo.simple "tracetree"
      , cargo.fromGithub "soenkehahn" "si" "master"
      ,     def
        //  { name = "vscode"
            , install =
                bash
                  ''
                  curl 'https://code.visualstudio.com/sha/download?build=stable&os=linux-x64' -Lo vscode.tar.gz
                  tar -xf vscode.tar.gz --no-same-owner
                  cd VSCode-linux-x64
                  mkdir -p ~/.local/opt/vscode
                  cp -r * ~/.local/opt/vscode/
                  cd ~/.local/bin/
                  ln -s ../opt/vscode/bin/code code
                  ''
            }
      ,     def
        //  { name = "ipfs"
            , install =
                bash
                  ''
                  curl -LO https://github.com/ipfs/kubo/releases/download/v0.17.0/kubo_v0.17.0_linux-amd64.tar.gz
                  tar --no-same-owner -xvf kubo_v0.17.0_linux-amd64.tar.gz
                  cp kubo/ipfs ~/.local/bin/
                  ''
            }
      ]
    }
