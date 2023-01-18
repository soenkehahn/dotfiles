let def = { skip = [] : List Text }

let bash = ./bash.dhall

let fetchRepo =
      \(repo : Text) ->
      \(ref : Text) ->
        ''
        git clone ${repo} --branch ${ref} --single-branch src
        cd src
        ''

in  [     def
      //  { name = "meson"
          , install =
              bash
                ''
                pip3 install --user meson ninja
                ''
          }
    ,     def
      //  { name = "seatd"
          , install =
              bash
                ''
                ${fetchRepo "https://git.sr.ht/~kennylevinsen/seatd" "0.7.0"}
                meson -Dprefix=$HOME/.local build
                ninja -C build
                ninja -C build install
                ''
          }
    ,     def
      //  { name = "wlroots"
          , install =
              bash
                ''
                ${fetchRepo
                    "https://gitlab.freedesktop.org/wlroots/wlroots"
                    "0.15.1"}
                export PKG_CONFIG_PATH=$HOME/.local/lib/x86_64-linux-gnu/pkgconfig
                meson -Dprefix=$HOME/.local -Dexamples=false build
                ninja -C build
                ninja -C build install
                ''
          }
    ,     def
      //  { name = "sway"
          , install =
              let version = "1.7"

              in  bash
                    ''
                    ${fetchRepo "https://github.com/swaywm/sway" "${version}"}
                    tree
                    sd 'Exec=sway' "Exec=zsh -l -c 'exec $HOME/.local/bin/sway'" sway.desktop
                    sd 'Name=Sway' "Name=My Sway (${version})" sway.desktop
                    export PKG_CONFIG_PATH=$HOME/.local/lib/x86_64-linux-gnu/pkgconfig
                    meson \
                      -Dprefix=$HOME/.local \
                      -Dwerror=false \
                      build
                    ninja -C build
                    ninja -C build install
                    ''
          }
    ]
