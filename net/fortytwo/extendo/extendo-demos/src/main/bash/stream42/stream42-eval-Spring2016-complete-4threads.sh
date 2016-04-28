#!/bin/bash

export JAVA_OPTIONS="-Xms8G -Xmx8G"

# first the whole hundreds
for t in 1 2 3 4; do
  for i in 100 200 300 400 500 600 700 800 900 1000; do
    for q in Friends Topics Friends,Topics; do
      ./stream42-eval-Spring2016.sh -l 60 -p ${i} -t ${t} -T 0 -q ${q}
    done
  done
done
# also fill in the *50's, time permitting
for t in 1 2 3 4; do
  for i in 50 150 250 350 450 550 650 750 850 950; do
    for q in Friends Topics Friends,Topics; do
      ./stream42-eval-Spring2016.sh -l 60 -p ${i} -t ${t} -T 0 -q ${q}
    done
  done
done
