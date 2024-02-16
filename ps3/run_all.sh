#!/bin/bash
cd test
FILES=$(ls *.fish)
for f in $FILES
do 
  echo "$f:"
  cat $f
  echo "-----------------------"
  cat ${f%.fish}.sexp
  echo "-----------------------"
  # ../ps3 src $f 2>&1 
  # Save the output to a .s file with the same base name
  ../ps3 sexp ${f%.fish}.sexp > ${f%.fish}.s 2>&1
done