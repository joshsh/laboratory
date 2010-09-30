This directory contains source code and programming notes for the experiments presented in the 2010 paper "Optimizing real-time RDF data streams".

The Common Lisp libraries for receiving and applying RDF transactions over UDP are contained in the three *.cl files.  Additional Lisp code used in the experiment is in ag_udp_transaction_throughput.txt.

The Java throughput-benchmarking code used on the Amazon EC2 sending machine is in the iptools Maven 2 project.

The Java code used for generating randomized "tweets" in JSON, parsing them to create RDF transaction XML, and sending them to the remote server in UDP datagrams is currently part of the TwitLogic code base:
    http://github.com/joshsh/twitlogic
See ThroughputTesting.java

The R code used for analysis of results and generation of the figures in the paper is in rdfstream_analysis_R.txt.

Please feel free to contact Joshua Shinavier (josh@fortytwo.net) with any questions.
