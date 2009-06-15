#!/usr/bin/env bash

set -e

for d in $*
  do
   echo $0: Testing $d
   cmp <(./hszpipe < $d) <(./zpipe < $d)
   cmp $d <(./zpipe < $d | ./zpipe -d) # just in case..
   cmp $d <(./zpipe < $d | ./hszpipe -d)
done