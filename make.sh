#!/bin/sh -ex

mkdir -p out
ghc -W -Wall -fno-warn-name-shadowing -outputdir out *.hs
