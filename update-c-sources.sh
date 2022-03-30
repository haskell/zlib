#!/usr/bin/env bash

set -e pipefail

zlib="$(curl http://zlib.net/ | rg '(zlib-\d+(\.\d+)*)\.tar\.gz' -o -r '$1' | head -n1)"

curl "http://zlib.net/${zlib}.tar.gz" -o "${zlib}.tar.gz"
tar xvf "${zlib}.tar.gz"

for csrc in $(ls -1 cbits)
do
    cp "${zlib}/${csrc}" cbits/
done
