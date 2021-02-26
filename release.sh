#! /usr/bin/env bash
set -euo pipefail

function build {
  cabal build
}

function package {
  local arch="$1"
  local dir="dist/$arch/"
  local zipName="uncertain-gantt.$arch.zip"
  (
    echo "Packaging arch $arch into $dir$zipName"
    [ -d "$dir" ] || mkdir -p "$dir"
    cp \
      "dist-newstyle/build/$arch/ghc-8.10.4/uncertain-gantt-0.0.0.0/x/uncertain-gantt/build/uncertain-gantt/uncertain-gantt" \
      "$dir"
    cp resources/example.ug "$dir"
    cd "$dir"
    [ -f "$zipName" ] && rm "$zipName" || true
    zip "$zipName" uncertain-gantt example.ug
  )
}
export -f package

build
ls dist-newstyle/build/ | \
  xargs -L1 -I{} bash -c 'package "{}"'
