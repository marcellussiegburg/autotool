#!/bin/bash

REPO=git://autolat.imn.htwk-leipzig.de/git

# ghc-7.2.* not working
# GHC=ghc-7.0.4
GHC=ghc-7.4.1

function work () {
    $* configure --with-ghc=$GHC --user
    $* build 
    $* haddock --with-haddock=haddock-$GHC --hyperlink-source --internal
    $* register --user
}

function prepare_lib () {
    git clone $REPO/autolib
    pushd autolib
    git checkout release
    popd
}

function build_lib () {
    pushd autolib
    work ./forauto cabal
    popd
}

function prepare_tool () {
    git clone $REPO/tool
    pushd tool
    git checkout release
    cp db/src/Mysqlconnect.hs.example db/src/Mysqlconnect.hs
    cp server/src/Config.hs.sample server/src/Config.hs
    popd
}

function build_tool () {
    pushd tool
    for dir in interface collection server client db
    do
	pushd $dir
	cabal install --user --only-dependencies
	work cabal
	popd
    done
    popd
}

# prepare_lib 
# build_lib 
# prepare_tool 
build_tool 
