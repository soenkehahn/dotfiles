all:
	cabal exec ghc -- xmonad.hs -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures -outputdir build -o xmonad-x86_64-linux -ilib
	! ack-grep --haskell undefined
