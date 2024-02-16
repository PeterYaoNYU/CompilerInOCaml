#!/bin/bash
cd test
FILES=`(ls *.fish)`
for f in $FILES
do 
  echo "$f:"
  cat $f
  echo "-----------------------"
  ../ps3 $f 2>&1 > ${f%.fish}.sexp
done
