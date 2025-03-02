let bash = ../bash.dhall

let simple = ../simple.dhall

let Package = { name : Text, skip : List Text, install : Text }

let skipPython =
      \(name : Text) ->
      \(install : Text) ->
        { name
        , skip = [ "/usr/local/lib/python3.10" ] : List Text
        , install = bash install
        }

let fetchRepo =
      \(repo : Text) ->
      \(ref : Text) ->
        ''
        git clone ${repo} --branch ${ref} --single-branch src
        cd src
        ''

in  { packages = [] : List Package
    }
