#!/usr/bin/env bash
set -e

version=$(date +"%y.%m.%d.%H.%M")

sudo docker run -v "$(pwd):/src" -v /tmp/stack:/root/.stack --rm leonti/haskell-static-build:17.09.28.00.34

sudo docker build -t leonti/rea-fetch:$version .
sudo docker push leonti/rea-fetch:$version

echo $version" is built"
