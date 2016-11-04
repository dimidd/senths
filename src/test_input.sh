#!/usr/bin/bash
ghc Main.hs -o senths
senths < ref.in > cur.out
diff ref.out cur.out
