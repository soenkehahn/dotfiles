let bash = ./bash.dhall

let cargoSkips =
      [ "~/.cargo"
      , "~/.rustup"
      , "~/.local/.crates.toml"
      , "~/.local/.crates2.json"
      ]

let simple =
      \(name : Text) ->
        { name
        , skip = cargoSkips
        , install =
            bash
              ''
              cargo install ${name} --root ~/.local
              ''
        }

let fromGithub =
      \(user : Text) ->
      \(repo : Text) ->
      \(ref : Text) ->
        { name = user ++ "/" ++ repo
        , skip = cargoSkips
        , install =
            let url = "https://github.com/" ++ user ++ "/" ++ repo

            in  bash
                  ''
                  git clone ${url} src
                  cd src
                  git checkout ${ref}
                  cargo install --path . --root ~/.local
                  ''
        }

in  { simple, fromGithub }
