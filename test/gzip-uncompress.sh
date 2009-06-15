#!/usr/bin/env bash

set -e
 
for d in $* 
  do
   echo $0: Testing $d
   gunzip -c < $d &> /dev/null && cmp <(./hsgunzip < $d) <(gunzip -c < $d)
done 