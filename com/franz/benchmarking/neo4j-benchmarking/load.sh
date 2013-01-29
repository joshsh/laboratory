#!/bin/bash

# Find Java
if [ "$JAVA_HOME" = "" ] ; then
	JAVA="java"
else
	JAVA="$JAVA_HOME/bin/java"
fi

# Set Java options
if [ "$JAVA_OPTIONS" = "" ] ; then
	JAVA_OPTIONS="-Xms1G -Xmx1G -d64 -server -XX:+UseConcMarkSweepGC"
fi

DIR=`dirname $0`

# Launch the application
$JAVA $JAVA_OPTIONS -cp $DIR/target/classes:$DIR/"target/dependency/*" com.franz.benchmarking.Neo4jLUBMLoader $*

# Return the program's exit code
exit $?
