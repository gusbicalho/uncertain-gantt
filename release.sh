#! /usr/bin/env bash
set -euo pipefail

cabal build
[ -d .release ] || mkdir .release
zip .release/uncertain-gantt.x86_64-osx.zip \
  dist-newstyle/build/x86_64-osx/ghc-8.10.3/uncertain-gantt-0.0.0.0/x/uncertain-gantt/build/uncertain-gantt/uncertain-gantt \
  resources/example.ug
