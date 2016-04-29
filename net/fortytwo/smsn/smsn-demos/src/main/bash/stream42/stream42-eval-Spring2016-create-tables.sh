#!/bin/bash

cd /tmp/stream42

for d in `ls -d *0`
do
  cd $d
  for i in `ls *.out`; do cat $i | grep STARTED > $i-STARTED; done
  for i in `ls *.out`; do cat $i | grep STOPPED > $i-STOPPED; done
  for i in `ls *.out`; do cat $i | grep TOTAL_RESULTS > $i-RESULTS; done
  for i in `ls *.out`; do cat $i | grep TOTAL_STATEMENTS > $i-STATEMENTS; done
  cd ..
done

cd ..