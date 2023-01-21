let simple = ../simple.dhall

let fetchRepo =
      \(repo : Text) ->
      \(ref : Text) ->
        ''
        git clone ${repo} --branch ${ref} --single-branch src
        cd src
        ''

in  { packages =
      [ simple
          "meson"
          ''
          pip3 install meson ninja
          ''
      , simple
          "seatd"
          ''
          ${fetchRepo "https://git.sr.ht/~kennylevinsen/seatd" "0.7.0"}
          meson build
          ninja -C build
          ninja -C build install
          ''
      , simple
          "wayland-protocols"
          ''
          ${fetchRepo
              "https://gitlab.freedesktop.org/wayland/wayland-protocols"
              "1.31"}
          meson build
          ninja -C build
          ninja -C build install
          ''
      , simple
          "wlroots"
          ''
          ${fetchRepo "https://gitlab.freedesktop.org/wlroots/wlroots" "0.16.1"}
          meson -Dexamples=false build
          ninja -C build
          ninja -C build install
          ''
      , let version = "1.8"

        in  simple
              "sway"
              ''
              ${fetchRepo "https://github.com/swaywm/sway" "${version}"}
              sed -i "s#Exec=sway#Exec=zsh -l -c 'exec /usr/local/bin/sway'#g" sway.desktop
              sed -i "s#Name=Sway#Name=My Sway \(${version}\)#g" sway.desktop
              meson -Dwerror=false build
              ninja -C build
              ninja -C build install
              ''
      , { name = "prettier"
        , skip = [ "/tmp", "/usr/local/share/.cache" ]
        , install =
            ''
            mkdir -p /usr/local/opt
            cd /usr/local/opt
            git clone https://github.com/prettier/prettier.git --branch 2.8.3 --single-branch
            cd prettier
            yarnpkg
            yarnpkg build
            ln -s ../opt/prettier/dist/bin-prettier.js /usr/local/bin/prettier
            ''
        }
      ]
    }
