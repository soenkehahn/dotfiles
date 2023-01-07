\ (script : Text) ->
  Text/replace "$script" script
    ''
    #!/usr/bin/env bash

    set -eux

    $script
    ''
