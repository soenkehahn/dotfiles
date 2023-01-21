let bash = ./bash.dhall

in  \(resolver : Text) ->
    \(name : Text) ->
      { name
      , skip = [ "~/.stack" ]
      , install =
          bash
            ''
            stack install --resolver=${resolver} ${name}
            ''
      }
