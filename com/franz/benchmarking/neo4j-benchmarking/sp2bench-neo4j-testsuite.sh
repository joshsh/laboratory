#!/bin/bash

# usage: ./sp2bench-neo4j-testsuite.sh [source N-triples file] [path to DB]

source=$1
db=$2

for i in `cat doc/cypher-queries.txt`; do rm -rf $2 \
    && time ./load-sp2bench.sh --source $1 --dest $2 \
    && time ./sp2bench-queries.sh $2 $i 1 10; done
