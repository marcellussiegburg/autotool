#!/bin/bash

LIBS="derive transport todoc output reader data tex reporter util algorithm dot relation fa genetic rewriting graph exp logic fta foa"

standalone-haddock \
    --hyperlink-source \
    -o /var/www/autotool/docs \
    --package-db $HOME/.ghc/x86_64-linux-7.8.3/package.conf.d \
    $(for lib in $LIBS ; do echo "../autolib/$lib" ; done ) \
    interface collection
