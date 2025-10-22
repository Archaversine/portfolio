#!/bin/bash 

cabal build
cabal exec site -- clean
cabal exec site -- build
cabal exec site -- watch
