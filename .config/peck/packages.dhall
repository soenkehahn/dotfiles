let bash = ./bash.dhall

let cargo = ./cargo.dhall

let def = { skip = [] : List Text }

in  { packages =
      [     def
        //  { name = "packager-test-executable"
            , install =
                bash
                  ''
                  mkdir -p ~/.local/bin/
                  echo "echo huhu" > ~/.local/bin/packager-test-executable
                  chmod +x ~/.local/bin/packager-test-executable
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
      , let stack =
              \(resolver : Text) ->
              \(name : Text) ->
                { name = "dhall-lsp-server"
                , skip = [ "~/.stack" ]
                , install =
                    bash
                      ( Text/replace
                          "\$name"
                          name
                          ( Text/replace
                              "\$resolver"
                              resolver
                              ''
                              stack install --resolver=$resolver $name
                              ''
                          )
                      )
                }

        in  stack "lts-19.30" "dhall-lsp-server"
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
      , cargo.fromGithub "soenkehahn" "si"
      , { name = "vscode"
        , skip = [ "~/.wget-hsts" ]
        , install =
            bash
              ''
              wget 'https://code.visualstudio.com/sha/download?build=stable&os=linux-x64' -O vscode.tar.gz
              tar -xvf vscode.tar.gz --no-same-owner
              cd VSCode-linux-x64
              mkdir -p ~/.local/opt/vscode
              cp -rv * ~/.local/opt/vscode/
              cd ~/.local/bin/
              ln -s ../opt/vscode/bin/code code
              ''
        }
      ]
    }
