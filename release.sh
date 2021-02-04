#! /usr/bin/env bash
set -euo pipefail

cabal build
[ -d dist ] || mkdir dist
cp dist-newstyle/build/x86_64-osx/ghc-8.10.3/uncertain-gantt-0.0.0.0/x/uncertain-gantt/build/uncertain-gantt/uncertain-gantt dist/
cp resources/example.ug dist/
cd dist
[ -f uncertain-gantt.x86_64-osx.zip ] && rm uncertain-gantt.x86_64-osx.zip || true
zip uncertain-gantt.x86_64-osx.zip uncertain-gantt example.ug
