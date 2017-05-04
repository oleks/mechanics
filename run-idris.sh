#!/usr/bin/env bash

set -euo pipefail

dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

docker run \
  --interactive --tty --rm \
  --volume "${dir}/:/home/idris/mechanics/" \
  --workdir "/home/idris/mechanics/idris/" \
  --entrypoint "/bin/ash" \
  oleks2/alpine-idris:1.0_0.5
