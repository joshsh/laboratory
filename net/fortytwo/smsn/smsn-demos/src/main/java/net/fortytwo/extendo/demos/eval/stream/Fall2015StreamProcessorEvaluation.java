package net.fortytwo.extendo.demos.eval.stream;

import net.fortytwo.rdfagents.model.Dataset;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.rdf.Activities;
import net.fortytwo.smsn.rdf.vocab.SmSnActivityOntology;
import net.fortytwo.smsn.rdf.vocab.Timeline;
import net.fortytwo.stream.StreamProcessor;
import net.fortytwo.stream.sparql.SparqlStreamProcessor;
import net.fortytwo.stream.sparql.impl.caching.CachingSparqlStreamProcessor;
import net.fortytwo.stream.sparql.impl.shj.SHJSparqlStreamProcessor;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.query.BindingSet;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Fall2015StreamProcessorEvaluation {
    private static final Logger logger = SemanticSynchrony.getLogger(Fall2015StreamProcessorEvaluation.class);

    private enum ProcessorClass { SHJ, Caching }

    private static final String
            DEFAULT_NS = "http://example.org/defaultNs/";

    private static final int HANDSHAKE_TTL = 1; // seconds; 1s is the minimum TTL

    public static final String QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES
            = "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?time ?actor1 ?actor2 ?person WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:recognitionTime ?instant .\n" +
            "  ?instant tl:at ?time .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?actor1 foaf:knows ?person .\n" +
            "  ?actor2 foaf:knows ?person .\n" +
            "  FILTER(str(?actor1) < str(?actor2))\n" +
            "}";

    // note: DBLP's dc:subject here instead of PKB's sioc:topic
    // note: SP2Bench's dc:creator here instead of PKB's foaf:maker
    public static final String QUERY_FOR_HANDSHAKE_COMMON_TOPICS
            = "PREFIX activity: <" + SmSnActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX dc: <http://purl.org/dc/elements/1.1/>\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?time ?actor1 ?actor2 ?topic WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:recognitionTime ?instant .\n" +
            "  ?instant tl:at ?time .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?paper1 dc:creator ?actor1 .\n" +
            "  ?paper1 dc:subject ?topic .\n" +
            "  ?paper2 dc:creator ?actor2 .\n" +
            "  ?paper2 dc:subject ?topic .\n" +
            "  FILTER(str(?actor1) < str(?actor2))\n" +
            "}";

    private static final String
            FRIENDS = "friends",
            TOPICS = "topics";

    private static final Random random;

    static {
        random = new Random();
        random.setSeed(getNow());
    }

    private final List<Resource> people = new ArrayList<Resource>();

    private SparqlStreamProcessor streamProcessor;
    private final List<EvalClient> clients;

    // average time between handshakes for a random individual
    // With a nonzero inter-event time, we try to simulate reality.
    // with zero inter-event time, we test the throughput of the system
    // the original system quickly became overloaded (the cleanup thread couldn't keep up)
    private int averageSecondsBetweenHandshakes;

    private ProcessorClass processorClass;
    private int totalPeople;
    private int totalThreads;
    private int timeLimitSeconds;
    private Set<String> queries;

    public Fall2015StreamProcessorEvaluation() {
        clients = new LinkedList<EvalClient>();
    }

    public void initialize()
            throws StreamProcessor.InvalidQueryException, IOException, StreamProcessor.IncompatibleQueryException,
            RDFParseException, RDFHandlerException {
        loadPeople();

        switch (processorClass) {
            case SHJ:
                streamProcessor = new SHJSparqlStreamProcessor();
                break;
            case Caching:
                streamProcessor = new CachingSparqlStreamProcessor();
                break;
            default:
                throw new IllegalStateException();
        }

        //streamProcessor.setDoPerformanceMetrics(true);

        int ppt = totalPeople/totalThreads;
        int extra = totalPeople - ppt*totalThreads;
        for (int i = 0; i < totalThreads; i++) {
            int length = (totalThreads - 1) == i ? ppt : ppt + extra;
            Resource[] peopleInThread = new Resource[length];
            for (int j = 0; j < length; j++) {
                peopleInThread[j] = people.get(i * ppt + j);
            }
            EvalClient client = new EvalClient(i, peopleInThread);
            clients.add(client);
        }

        // first, add queries
        if (queries.contains(FRIENDS)) {
            logger.info("adding 'friends in common' query");
            streamProcessor.addQuery(StreamProcessor.INFINITE_TTL, QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES,
                    new BiConsumer<BindingSet, Long>() {
                        @Override
                        public void accept(BindingSet solution, Long expirationTime) {
                            Value person = solution.getValue("person");
                            handleHandshakeResult(solution, FRIENDS, person);
                        }
                    });
        }
        if (queries.contains(TOPICS)) {
            logger.info("adding 'topics in common' query");
            streamProcessor.addQuery(StreamProcessor.INFINITE_TTL, QUERY_FOR_HANDSHAKE_COMMON_TOPICS,
                    new BiConsumer<BindingSet, Long>() {
                        @Override
                        public void accept(BindingSet solution, Long expirationTime) {
                            Value topic = solution.getValue("topic");
                            handleHandshakeResult(solution, TOPICS, topic);
                        }
                    });
        }

        // add data after queries
        addStaticData(totalPeople);
    }

    private void loadPeople() throws IOException {
        File f = new File("/tmp/stream42/people.txt");
        if (!f.exists()) {
            throw new IllegalStateException();
        }
        InputStream in = new FileInputStream(f);
        try {
            BufferedReader br = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = br.readLine())) {
                //String[] a = line.trim().split(":");
                //if (2 != a.length) {
                //    throw new IllegalStateException();
                //}
                URI p = new URIImpl("urn:bnode:" + line.trim());
                people.add(p);
                //people.add(new BNodeImpl(a[1]));
            }
        } finally {
            in.close();
        }
    }

    // note: this method may be called in a thread other than the one in which the
    // simulation loop is running.
    private void handleHandshakeResult(BindingSet bindingSet, String queryName, Value... otherValues) {
        long now = getNow();
        long timestamp;

        Value actor1 = bindingSet.getValue("actor1");
        Value actor2 = bindingSet.getValue("actor2");

        // always output the actors in lexicographic order
        if (actor1.stringValue().compareTo(actor2.stringValue()) > 0) {
            Value tmp = actor1;
            actor1 = actor2;
            actor2 = tmp;
        }

        Value timeValue = bindingSet.getValue("time");
        if (null == timeValue) {
            logger.severe("no time in " + bindingSet);
            return;
        }
        try {
            timestamp = Activities.TIMESTAMP_FORMAT.parse(timeValue.stringValue()).getTime();
        } catch (Exception t) {
            logger.log(Level.WARNING, "count not parse as dateTime: " + timeValue.stringValue()
                    + " in solution " + bindingSet);
            return;
        }

        StringBuilder sb = new StringBuilder();
        sb.append(now).append("\t").append("MATCH\t").append(queryName).append("\t").append(timestamp).append("\t")
                .append(actor1.stringValue()).append("\t").append(actor2.stringValue());
        for (Value v : otherValues) {
            sb.append("\t").append(v.stringValue());
        }
        System.out.println(sb);
    }

    private void addStaticData(int totalPeople) throws IOException, RDFParseException, RDFHandlerException {
        long before = getNow();

        RDFParser p = Rio.createParser(RDFFormat.NTRIPLES);
        p.setRDFHandler(streamProcessor.createRDFHandler(0));

        File dir = new File("/tmp/stream42/" + totalPeople);
        if (!dir.exists() || !dir.isDirectory()) {
            throw new IllegalStateException();
        }

        for (File f : dir.listFiles()) {
            if (f.getName().endsWith(".nt")) {
                logger.info("loading " + f);
                InputStream in = new FileInputStream(f);
                try {
                    p.parse(in, DEFAULT_NS);
                } finally {
                    in.close();
                }
            }
        }

        long after = getNow();
        logger.info("loaded static dataset in " + (after - before) + "ms");
    }

    public void runSimulation() throws InterruptedException {
        for (EvalClient client : clients) {
            new Thread(client).start();
        }

        // assuming starting the clients takes no time
        Thread.sleep(0 < timeLimitSeconds ? timeLimitSeconds * 1000L : Long.MAX_VALUE);

        for (EvalClient client : clients) {
            client.stop();
        }
    }

    public void shutDown() {
        streamProcessor.shutDown();
    }

    /**
     * @param frequency average frequency of events, in 1/milliseconds
     * @return time until the next event, in milliseconds
     */
    private long timeToNextEvent(double frequency) {
        return (long) (-Math.log(1.0 - random.nextDouble()) / frequency);
    }

    private static Statement[] toArray(final Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    private static void mainPrivate(final String[] args)
            throws StreamProcessor.IncompatibleQueryException, IOException, StreamProcessor.InvalidQueryException,
            InterruptedException, RDFParseException, RDFHandlerException {

        Options options = new Options();

        Option classOpt = new Option("c", "class", true, "stream processor class (SHJ/Caching, default: SHJ)");
        classOpt.setArgName("CLASS");
        classOpt.setRequired(false);
        options.addOption(classOpt);

        Option threadsOpt = new Option("t", "threads", true, "number of worker threads (default: 1)");
        threadsOpt.setArgName("THREADS");
        threadsOpt.setRequired(false);
        options.addOption(threadsOpt);

        Option peopleOpt = new Option("p", "people", true, "total number of people (default: 100)");
        peopleOpt.setArgName("PEOPLE");
        peopleOpt.setRequired(false);
        options.addOption(peopleOpt);

        Option queriesOpt = new Option("q", "queries", true, "queries (default: friends,topics)");
        queriesOpt.setArgName("QUERIES");
        queriesOpt.setRequired(false);
        options.addOption(queriesOpt);

        Option shakeTimeOpt = new Option("T", "timeBetweenShakes", true,
                "average time between handshakes, in seconds (default: 180)");
        shakeTimeOpt.setArgName("SECONDS");
        shakeTimeOpt.setRequired(false);
        options.addOption(shakeTimeOpt);

        Option limitOpt = new Option("l", "limit", true, "time limit in seconds");
        limitOpt.setArgName("SECONDS");
        limitOpt.setRequired(false);
        options.addOption(limitOpt);

        Option verboseOpt = new Option("v", "verbose", false, "verbose output");
        verboseOpt.setRequired(false);
        options.addOption(verboseOpt);

        CommandLineParser clp = new PosixParser();
        CommandLine cmd = null;

        try {
            cmd = clp.parse(options, args);
        } catch (org.apache.commons.cli.ParseException e) {
            printUsageAndExit(options);
        }

        ProcessorClass processorClass = ProcessorClass.valueOf(cmd.getOptionValue(classOpt.getOpt(), "SHJ"));
        int nThreads = Integer.valueOf(cmd.getOptionValue(threadsOpt.getOpt(), "1"));
        int totalPeople = Integer.valueOf(cmd.getOptionValue(peopleOpt.getOpt(), "100"));
        String queriesStr = cmd.getOptionValue(queriesOpt.getOpt(), "friends,topics");
        int timeBetweenShakes = Integer.valueOf(cmd.getOptionValue(limitOpt.getOpt(), "0"));
        int averageSecondsBetweenHandshakes = Integer.valueOf(cmd.getOptionValue(shakeTimeOpt.getOpt(), "180"));
        boolean verbose = cmd.hasOption(verboseOpt.getOpt());

        Set<String> queries = new HashSet<String>();
        for (String q : queriesStr.split(",")) {
            String query = q.trim();
            if (0 == query.length()) {
                printUsageAndExit(options);
            }
            queries.add(query);
        }

        Fall2015StreamProcessorEvaluation eval = new Fall2015StreamProcessorEvaluation();
        //eval.peoplePerThread = 100;
        //eval.totalThreads = 1;
        //eval.averageSecondsBetweenHandshakes = 3 * 60;
        //eval.timeLimitSeconds = 5 * 60;
        //eval.queries.add(FRIENDS);
        //eval.queries.add(TOPICS);
        eval.processorClass = processorClass;
        eval.totalPeople = totalPeople;
        eval.totalThreads = nThreads;
        eval.averageSecondsBetweenHandshakes = averageSecondsBetweenHandshakes;
        eval.timeLimitSeconds = timeBetweenShakes;
        eval.queries = queries;

        eval.initialize();
        eval.runSimulation();
        eval.shutDown();

        logger.info("exiting simulation");
    }

    private static void printUsageAndExit(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("sesamestream-eval", options);
        //System.out.println("options: " + options.toString());
        //System.err.println("see source for usage");
        System.exit(1);
    }

    private class EvalClient implements Runnable {
        private final int id;
        private long lastEvent;
        private boolean stopped;
        private final Resource[] localPeople;
        private final double averageFrequency;

        private EvalClient(final int id,
                           final Resource[] localPeople) {
            logger.info("creating client " + id + " with " + localPeople.length + " people");
            this.id = id;
            this.localPeople = localPeople;

            averageFrequency = 0 == averageSecondsBetweenHandshakes
                ? 0 : localPeople.length / (2.0 * averageSecondsBetweenHandshakes * 1000);
        }

        private void shake(long now) throws IOException {
            // choose two random, distinct people from this group.
            // They can be the same as a previous pair.
            int pid1 = random.nextInt(localPeople.length);
            int pid2 = (pid1 + 1 + random.nextInt(localPeople.length - 1)) % localPeople.length;

            Resource actor1 = localPeople[pid1];
            Resource actor2 = localPeople[pid2];

            // consistent ordering of actors is convenient for analysis
            if (actor1.stringValue().compareTo(actor2.stringValue()) > 0) {
                Resource tmp = actor1;
                actor1 = actor2;
                actor2 = tmp;
            }

            // note: the logged timestamp is identical to the event timestamp represented in RDF
            System.out.println(now + "\tEVENT\t" + this.id + "\t"
                    + actor1.stringValue() + "\t" + actor2.stringValue());

            Dataset d = Activities.datasetForHandshakeInteraction(now, actor1, actor2);
            streamProcessor.addInputs(HANDSHAKE_TTL, toArray(d));
        }

        private void waitAndShake() throws InterruptedException, IOException {
            if (0 != averageFrequency) {
                // use clock time as simulation time
                long now = getNow();

                long delay = timeToNextEvent(averageFrequency);
                long nextEvent = lastEvent + delay;
                if (lastEvent > 0) {
                    if (nextEvent > now) {
                        Thread.sleep(nextEvent - now);
                    } else {
                        // note the ratio of DELAY_TOO_SHORT to EVENT
                        System.out.println(now + "\tDELAY_TOO_SHORT\t" + this.id + "\t" + lastEvent + "\t" + delay);
                    }
                }
            }

            // note: this may not be exactly equal to nextEvent
            lastEvent = getNow();

            shake(lastEvent);
        }

        public void run() {
            try {
                while (!stopped) {
                    waitAndShake();
                }
            } catch (Exception t) {
                logger.log(Level.SEVERE, "eval client failed", t);
            }
            logger.info("eval client " + id + " exited");
        }

        public void stop() {
            long now = getNow();
            System.out.println(now + '\t' + id + "\tSTOPPED");
            stopped = true;
        }
    }

    private static long getNow() {
        return System.currentTimeMillis();
    }

    /*
      ./new-sesamestream-evaluation.sh -l 30 -p 100 -t 2 -q friends
     */
    public static void main(final String[] args) {
        try {
            mainPrivate(args);
        } catch (Exception t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }
}
