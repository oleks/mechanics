#!/usr/bin/env bash

set -euo pipefail

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

docker run \
  --interactive --tty --rm \
  --volume "${dir}/../:/home/ghc/mechanics/" \
  --workdir "/home/ghc/mechanics/haskell/" \
  oleks2/alpine-haskell-pl-dev:8.0.2_0.2
