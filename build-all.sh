#!/bin/bash

REPO=git://autolat.imn.htwk-leipzig.de/git

# latest stable 
AUTOLIBTAG=december-2011
TOOLTAG=c9e220d48c21fd38c21ed4bc058470eb5b9e2446

# ghc-7.2.* not working
GHC=ghc-7.0.4

function cabi {
    cabal configure --with-ghc=$GHC
    cabal build
    cabal haddock --hyperlink-source
    cabal install
}

git clone $REPO/autolib
pushd autolib
git checkout $AUTOLIBTAG
./forauto cabi .
popd

git clone $REPO/tool
pushd tool
git checkout $TOOLTAG

cp db/src/Mysqlconnect.hs.example db/src/Mysqlconnect.hs
cp server/src/Config.hs.sample server/src/Config.hs

for dir in interface collection server client db
do
    pushd $dir
    cabi .
    popd
done
popd

