#!/bin/bash

if [ "$JAVA_HOME" = "" ] ; then
	JAVA="java"
else
	JAVA="$JAVA_HOME/bin/java"
fi

if [ "$JAVA_OPTIONS" = "" ] ; then
	JAVA_OPTIONS="-Xms32M -Xmx512M"
fi

LINK=`readlink $0`
if [ "$LINK" ]; then
    DIR=`dirname $LINK`
else
    DIR=`dirname $0`
fi

DIR=${DIR}/../../../..
exec $JAVA $JAVA_OPTIONS -cp ${DIR}/target/classes:${DIR}/"target/dependency/*" net.fortytwo.extendo.demos.eval.stream.SesameStreamEvaluation $*
