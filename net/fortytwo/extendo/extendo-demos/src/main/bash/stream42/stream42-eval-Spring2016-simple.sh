#!/bin/bash

LINK=`readlink $0`
if [ "$LINK" ]; then
    DIR=`dirname ${LINK}`
else
    DIR=`dirname $0`
fi

export JAVA_OPTIONS="-Xms8G -Xmx8G"

for t in 1; do
  for i in 100 200 300 400 500 600 700 800 900 1000; do
    for q in Friends Topics Friends,Topics; do
      ${DIR}/stream42-eval-Spring2016.sh -l 20 -p ${i} -t ${t} -T 0 -q ${q}
    done
  done
done
