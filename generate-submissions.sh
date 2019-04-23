#!/bin/sh

rm -rf submissions
mkdir submissions

cd exercises

for i in */
do
  zip -r ../submissions/Sheet_${i%/}_${MAT1}_${MAT2}_${MAT3}_${MAT4}.zip ${i}
done
