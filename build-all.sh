#!/bin/bash

REPO=git://autolat.imn.htwk-leipzig.de/git

# ghc-7.2.* not working
GHC=ghc-7.0.4

function work () {
    $* configure --with-ghc=$GHC
    $* build
    $* haddock-$GHC --hyperlink-source
    $* install
}

git clone $REPO/autolib
pushd autolib
git checkout release
work ./forauto cabal
popd

git clone $REPO/tool
pushd tool
git checkout release

cp db/src/Mysqlconnect.hs.example db/src/Mysqlconnect.hs
cp server/src/Config.hs.sample server/src/Config.hs

for dir in interface collection server client db
do
    pushd $dir
    work cabal
    popd
done
popd

