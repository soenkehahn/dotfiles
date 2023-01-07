let bash = ./bash.dhall

let cargoSkips = [
  "~/.cargo",
  "~/.rustup",
  "~/.local/.crates.toml",
  "~/.local/.crates2.json",
]

let simple = \ (name : Text) -> {
  name,
  skip = cargoSkips,
  install = bash (Text/replace "$package" name
    ''
    cargo install $package --root ~/.local
    ''),
}

let fromGithub = \ (user : Text) -> \ (repo : Text) -> {
  name = user ++ "/" ++ repo,
  skip = cargoSkips,
  install = bash (Text/replace "$url" ("https://github.com/" ++ user ++ "/" ++ repo)
    ''
    git clone $url checkout
    cd checkout
    cargo install --path . --root ~/.local
    ''),
}
in {simple, fromGithub}
