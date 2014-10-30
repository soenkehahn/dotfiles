set -o errexit

ghc xmonad.hs -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures -Werror -outputdir /tmp/xmonad_builds -o xmonad-x86_64-linux -ilib
ack-grep --haskell undefined && false
# xmonad --restart
