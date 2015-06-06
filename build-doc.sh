#!/bin/bash

LIBS="derive transport todoc output reader data tex reporter util algorithm dot relation fa genetic rewriting graph exp logic fta foa"

GHC=7.8.4

echo IF THIS FAILS STRANGELY: check the hard-coded ghc version in the script

standalone-haddock \
    --hyperlink-source \
    -o /var/www/autotool/docs \
    --package-db $HOME/.ghc/x86_64-linux-$GHC/package.conf.d \
    $(for lib in $LIBS ; do echo "../autolib/$lib" ; done ) \
    interface collection
