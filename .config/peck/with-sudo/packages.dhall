let def = { skip = [] : List Text }

let bash = ../bash.dhall

let fetchRepo =
      \(repo : Text) ->
      \(ref : Text) ->
        ''
        git clone ${repo} --branch ${ref} --single-branch src
        cd src
        ''

in  { packages =
      [     def
        //  { name = "meson"
            , install =
                bash
                  ''
                  pip3 install meson ninja
                  ''
            }
      ,     def
        //  { name = "seatd"
            , install =
                bash
                  ''
                  ${fetchRepo "https://git.sr.ht/~kennylevinsen/seatd" "0.7.0"}
                  meson build
                  ninja -C build
                  ninja -C build install
                  ''
            }
      ,     def
        //  { name = "wayland-protocols"
            , install =
                bash
                  ''
                  ${fetchRepo
                      "https://gitlab.freedesktop.org/wayland/wayland-protocols"
                      "1.31"}
                  tree
                  ls -la
                  meson build
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
                      "0.16.1"}
                  meson -Dexamples=false build
                  ninja -C build
                  ninja -C build install
                  ''
            }
      ,     def
        //  { name = "sway"
            , install =
                let version = "1.8"

                in  bash
                      ''
                      ${fetchRepo "https://github.com/swaywm/sway" "${version}"}
                      sed -i "s#Exec=sway#Exec=zsh -l -c 'exec /usr/local/bin/sway'#g" sway.desktop
                      sed -i "s#Name=Sway#Name=My Sway \(${version}\)#g" sway.desktop
                      ls -la
                      cat sway.desktop
                      meson -Dwerror=false build
                      ninja -C build
                      ninja -C build install
                      ''
            }
      ]
    }
