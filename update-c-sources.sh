#!/bin/sh

set -e pipefail

curl -s http://zlib.net/zlib.tar.gz | tar xz

for csrc in cbits/*
do
    mv zlib-*/"${csrc##*/}" cbits/
done

rm -rf zlib-*
