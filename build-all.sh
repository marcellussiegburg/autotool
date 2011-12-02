#!/bin/bash

REPO=git://autolat.imn.htwk-leipzig.de/git

# latest stable build
TAG=december-2011

# ghc-7.2.* not working
GHC=ghc-7.0.4

CABIFLAGS="--with-ghc=$GHC --enable-documentation"

git clone $REPO/autolib
pushd autolib
git checkout $TAG
./forauto cabal install $CABIFLAGS
popd

git clone $REPO/tool
pushd tool
git checkout $TAG
cp db/src/Mysqlconnect.hs.example db/src/Mysqlconnect.hs
for dir in interface collection server client trial super
do
    pushd $dir
    cabal install $CABIFLAGS
    popd
done
popd

