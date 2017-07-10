#!/bin/bash

set -e

stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts -O"
stack exec -- rea-fetch +RTS -p -h
hp2ps rea-fetch.hp
