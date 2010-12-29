


java -cp target/droidspeak-*-full.jar jade.Boot -agents mine:net.fortytwo.droidspeak.jade.MyFirstAgent

java -cp target/droidspeak-*-full.jar jade.Boot -agents "foo:net.fortytwo.droidspeak.jade.FooAgent;bar:net.fortytwo.droidspeak.jade.BarAgent"

java -cp target/droidspeak-*-full.jar jade.Boot -name droidspeak -port 1331 -agents "timer:net.fortytwo.droidspeak.jade.TimerAgent;echo:net.fortytwo.droidspeak.jade.EchoAgent;dictation:net.fortytwo.droidspeak.jade.DictationAgent"



java -cp target/droidspeak-*-full.jar net.fortytwo.droidspeak.jade -name droidspeak -agents "timer:net.fortytwo.droidspeak.jade.TimerAgent;echo:net.fortytwo.droidspeak.jade.EchoAgent;dictation:net.fortytwo.droidspeak.jade.DictationAgent"



sudo iptables -I INPUT -p tcp -m tcp --dport 7778 -j ACCEPT

