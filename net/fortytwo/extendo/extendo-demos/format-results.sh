#!/bin/bash
echo "l <- c()
"
cat /tmp/eval.txt|grep latency|grep pulse|sed 's/.*=.//'|sed 's/ms.*//'|tr '\n' ','|sed 's/^/p <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt|grep latency|grep friends-handshake|sed 's/.*=.//'|sed 's/ms.*//'|tr '\n' ','|sed 's/^/fl <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt|grep latency|grep topics-handshake|sed 's/.*=.//'|sed 's/ms.*//'|tr '\n' ','|sed 's/^/tl <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep -i removed|sed 's/.*and.//'|sed 's/.solutions.*//'|tr '\n' ','|sed 's/^/r <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep dwell|sed 's/.*after.//'|sed 's/s.*//'|tr '\n' ','|sed 's/^/dt <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep idle|sed 's/.*after.//'|sed 's/s.*//'|tr '\n' ','|sed 's/^/it <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep took|sed 's/.*took.//'|sed 's/ms//'|tr '\n' ','|sed 's/^/cycle <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep spent|sed 's/spent at most //'|sed 's/ms on moves and /,/'|sed 's/ms.*//'|tr '\n' ','|sed 's/^/spent <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^moves this cycle"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/moves <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^pulses this cycle"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/pulses <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^received pulses this cycle"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/received.pulses <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^true received pulses this cycle"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/true.received.pulses <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^handshakes this cycle"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/handshakes <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^received common-knows"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/received.shakes.knows <- c\(/'|sed 's/.$/\)/'
echo ""
cat /tmp/eval.txt |grep "^received common-topics"|sed 's/null/0/'|sed 's/[^0-9]*//'|sed 's/[^0-9].*//'|tr '\n' ','|sed 's/^/received.shakes.topics <- c\(/'|sed 's/.$/\)/'
echo "
missed <- (sum(pulses) - sum(true.received.pulses))/sum(pulses);
i <- 1:(length(spent)/2)
mean(p)
mean(fl)
mean(tl)
mean(r)
mean(dt)
mean(it)
mean(cycle)
mean(moves)
mean(pulses)
mean(received.pulses)
missed
mean(spent[i*2]) / mean(spent[i*2-1])
"

