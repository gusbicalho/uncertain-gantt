#! /usr/bin/env bash
set -euo pipefail

cabal build
[ -d dist ] || mkdir dist
zip dist/uncertain-gantt.x86_64-osx.zip \
  dist-newstyle/build/x86_64-osx/ghc-8.10.3/uncertain-gantt-0.0.0.0/x/uncertain-gantt/build/uncertain-gantt/uncertain-gantt \
  resources/example.ug
