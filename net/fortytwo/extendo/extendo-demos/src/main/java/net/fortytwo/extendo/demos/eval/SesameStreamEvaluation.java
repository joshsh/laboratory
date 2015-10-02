package net.fortytwo.extendo.demos.eval;

import edu.rpi.twc.rdfstream4j.BindingSetHandler;
import edu.rpi.twc.rdfstream4j.QueryEngine;
import edu.rpi.twc.rdfstream4j.impl.QueryEngineImpl;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.rdf.Activities;
import net.fortytwo.extendo.rdf.vocab.ExtendoActivityOntology;
import net.fortytwo.extendo.rdf.vocab.FOAF;
import net.fortytwo.extendo.rdf.vocab.Timeline;
import net.fortytwo.rdfagents.model.Dataset;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.ValueFactory;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.model.vocabulary.OWL;
import org.openrdf.model.vocabulary.RDF;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A simulation of a variable number of people moving between a variable number of rooms, as in a conference center
 * equipped with Wi-Fi triangulation, occasionally shaking hands with other conference-goers.
 * A SesameStream continuous SPARQL query engine combines event data from individuals to recognize possible handshakes
 * and identify relationships between the conference-goers.
 * The task of event processing is broken up by room to a variable number of worker threads.
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SesameStreamEvaluation {
    private static final Logger logger = Extendo.getLogger(SesameStreamEvaluation.class);

    private static final String EX = "http://example.org/";

    private static final int
            QUERY_TTL = 0,
            MAX_HANDSHAKE_PULSE_TTL = 5,
            HANDSHAKE_TTL = 3;

    private static final int
            MAX_PAPERS_PER_PERSON = 20,
            MAX_TOPICS_PER_PAPER = 5;

    private static final long CYCLE_LENGTH_WARN_THRESHOLD = 1000L;

    private final long
            averageMillisecondsBetweenMoves,
            averageMillisecondsBetweenHandshakes;

    private final int presenceTtl;

    // a set of handshakes, accessed by multiple threads, for distinguishing actual shakes from false positives
    private Set<String> handshakesInProgress = newConcurrentSet();

    // e.g. 2015-02-22T01:35:10-0500
    private static final DateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'hh:mm:ssZ");

    //*
    private static final String QUERY_FOR_HANDSHAKE_PAIRS
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "SELECT ?actor1 ?actor2 ?room ?time1 ?time2 WHERE {\n" +
            "  ?a1 a activity:HandshakePulse .\n" +
            "  ?a1 activity:actor ?actor1 .\n" +
            "  ?a1 activity:recognitionTime ?instant1 .\n" +
            "  ?instant1 tl:at ?time1 .\n" +
            "  ?a2 a activity:HandshakePulse .\n" +
            "  ?a2 activity:actor ?actor2 .\n" +
            "  ?a2 activity:recognitionTime ?instant2 .\n" +
            "  ?instant2 tl:at ?time2 .\n" +
            "  ?actor1 activity:locatedAt ?room .\n" +
            "  ?actor2 activity:locatedAt ?room .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";
    //*/

    private static final String QUERY_FOR_HANDSHAKES
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "  PREFIX tl: <" + Timeline.NAMESPACE + ">\n" +
            "  SELECT ?actor1 ?actor2 ?time WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?a activity:recognitionTime ?instant .\n" +
            "  ?instant tl:at ?time .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";

    private static final String QUERY_FOR_HANDSHAKE_COMMON_TOPICS
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX sioc: <http://rdfs.org/sioc/ns#>\n" +
            "SELECT ?actor1 ?actor2 ?topic WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?paper1 foaf:maker ?actor1 .\n" +
            "  ?paper1 sioc:topic ?topic .\n" +
            "  ?paper2 foaf:maker ?actor2 .\n" +
            "  ?paper2 sioc:topic ?topic .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";

    private static final String QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES
            = "PREFIX activity: <" + ExtendoActivityOntology.NAMESPACE + ">\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "SELECT ?actor1 ?actor2 ?person WHERE {\n" +
            "  ?a a activity:Handshake .\n" +
            "  ?a activity:actor ?actor1 .\n" +
            "  ?a activity:actor ?actor2 .\n" +
            "  ?actor1 foaf:knows ?person .\n" +
            "  ?actor2 foaf:knows ?person .\n" +
            "  FILTER(?actor1 != ?actor2)\n" +
            "}";

    private final QueryEngine queryEngine;
    private final ValueFactory vf;

    private final Random random = new Random();

    private final Person[] people;
    private final Room[] rooms;
    private final Topic[] topics;

    private ThreadLocal<Long>
            timeOfLastPulse = new ThreadLocal<Long>(),
            timeOfLastHandshake = new ThreadLocal<Long>(),
            timeOfPulseTrigger = new ThreadLocal<Long>(),
            timeOfFriendsHandshakeTrigger = new ThreadLocal<Long>(),
            timeOfTopicsHandshakeTrigger = new ThreadLocal<Long>();

    private ThreadLocal<Integer> countOfMoves = new ThreadLocal<Integer>();
    private ThreadLocal<Integer> countOfPulses = new ThreadLocal<Integer>();
    private ThreadLocal<Integer> countOfHandshakes = new ThreadLocal<Integer>();
    private ThreadLocal<Integer> countOfReceivedPulses = new ThreadLocal<Integer>();
    private ThreadLocal<Integer> countOfTrueReceivedPulses = new ThreadLocal<Integer>();
    private ThreadLocal<Integer> countOfReceivedHandshakesWithCommonKnows = new ThreadLocal<Integer>();
    private ThreadLocal<Integer> countOfReceivedHandshakesWithCommonTopics = new ThreadLocal<Integer>();

    private ThreadLocal<List<Long>> pulseLatency = new ThreadLocal<List<Long>>();
    private ThreadLocal<List<Long>> friendsHandshakeLatency = new ThreadLocal<List<Long>>();
    private ThreadLocal<List<Long>> topicsHandshakeLatency = new ThreadLocal<List<Long>>();

    private final Object printMutex = "";

    private final boolean verbose;

    private void addToSample(final ThreadLocal<List<Long>> sample,
                             final Long time) {
        List<Long> list = sample.get();
        if (null == list) {
            list = new LinkedList<Long>();
            sample.set(list);
        }
        list.add(time);
    }

    public SesameStreamEvaluation(final boolean verbose,
                                  final int totalThreads,
                                  final int totalPeople,
                                  final int totalRooms,
                                  final Set<String> queries,
                                  final int moveTime,
                                  final int shakeTIme,
                                  final int timeLimitSeconds,
                                  final double probTopicsInCommon)
            throws QueryEngine.InvalidQueryException, IOException, QueryEngine.IncompatibleQueryException {

        this.verbose = verbose;

        if (totalThreads < 1) {
            throw new IllegalArgumentException("invalid number of threads: " + totalThreads);
        }

        if (timeLimitSeconds < 0) {
            throw new IllegalArgumentException();
        }

        if (totalRooms < 2) {
            throw new IllegalArgumentException("simulation requires at least two rooms");
        }

        if (probTopicsInCommon <= 0) {
            throw new IllegalArgumentException("probability of common topics must be > 0");
        }

        averageMillisecondsBetweenMoves = moveTime * 1000L;
        averageMillisecondsBetweenHandshakes = shakeTIme * 1000L;

        presenceTtl = moveTime;

        // these give around 50% probability of a common acquaintance
        int[] x = new int[]{0, 100, 200, 500, 1000};
        int[] y = new int[]{0, 8, 12, 18, 25};
        Integer p = null;
        for (int i = 0; i < x.length; i++) {
            if (x[i] == totalPeople) {
                p = y[i];
                break;
            } else if (x[i] > totalPeople) {
                p = y[i - 1] + (int) (((totalPeople - x[i]) / (1.0 * (x[i] - x[i - 1]))) * (y[i] - y[i - 1]));
                break;
            }
        }

        if (null != p && p <= 0) {
            throw new IllegalStateException("total people are less than expected...");
        }
        int maxPeopleKnown = null != p
                ? p
                : (int) (y[x.length - 1] * totalPeople / (1.0 * x[x.length - 1]));

        //SesameStream.setDoPerformanceMetrics(true);

        queryEngine = new QueryEngineImpl();
        vf = new ValueFactoryImpl();

        queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_PAIRS, new BindingSetHandler() {
            @Override
            public void handle(BindingSet bindingSet) {
                increment(countOfReceivedPulses);

                long now = System.currentTimeMillis();
                long time1;
                Value timeValue = bindingSet.getValue("time1");
                if (null == timeValue) {
                    logger.warning("no time1 in " + bindingSet);
                    return;
                }
                try {
                    time1 = DATE_FORMAT.parse(timeValue.stringValue()).getTime();
                } catch (Throwable t) {
                    logger.log(Level.WARNING, "count not parse as dateTime: " + timeValue.stringValue()
                            + " in solution " + bindingSet);
                    return;
                }

                String actor1 = bindingSet.getValue("actor1").stringValue();
                String actor2 = bindingSet.getValue("actor2").stringValue();
                String a1 = actor1.substring(actor1.lastIndexOf("n") + 1);
                String a2 = actor2.substring(actor2.lastIndexOf("n") + 1);

                if (Integer.valueOf(a1).compareTo(Integer.valueOf(a2)) > 0) {
                    String tmp = a1;
                    a1 = a2;
                    a2 = tmp;
                }
                String key = "" + time1 + ":" + a1 + ":" + a2;

                if (handshakesInProgress.remove(key)) {
                    increment(countOfTrueReceivedPulses);

                    findPulseLatency(now);

                    if (verbose) {
                        System.out.println("handshake pair: " + bindingSet);
                    }
                    Person person1 = people[Integer.valueOf(a1)];
                    Person person2 = people[Integer.valueOf(a2)];
                    try {
                        handshake(person1, person2);
                    } catch (IOException e) {
                        throw new IllegalStateException(e);
                    }
                } else if (verbose) {
                    System.out.println("no such handshake: " + key);
                }
            }
        });
        queries.remove("none");

        if (queries.contains("friends")) {
            queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_COMMON_ACQUAINTANCES, new BindingSetHandler() {
                @Override
                public void handle(BindingSet bindingSet) {
                    increment(countOfReceivedHandshakesWithCommonKnows);

                    findFriendsHandshakeLatency(System.currentTimeMillis());

                    if (verbose) {
                        System.out.println("GOT A 'KNOWS' HANDSHAKE: " + bindingSet);
                    }
                }
            });
            queries.remove("friends");
        }

        if (queries.contains("topics")) {
            queryEngine.addQuery(QUERY_TTL, QUERY_FOR_HANDSHAKE_COMMON_TOPICS, new BindingSetHandler() {
                @Override
                public void handle(BindingSet bindingSet) {
                    increment(countOfReceivedHandshakesWithCommonTopics);

                    findTopicsHandshakeLatency(System.currentTimeMillis());

                    if (verbose) {
                        System.out.println("GOT A 'TOPICS' HANDSHAKE: " + bindingSet);
                    }
                }
            });
            queries.remove("topics");
        }

        if (queries.size() > 0) {
            throw new IllegalArgumentException("some or all queries not supported");
        }

        double averagePeopleKnown = (1 + maxPeopleKnown) / 2;
        double averagePapersPerPerson = (1 + MAX_PAPERS_PER_PERSON) / 2;
        double averageTopicsPerPaper = (1 + MAX_TOPICS_PER_PAPER) / 2;
        double averageTopicsPerPerson = averagePapersPerPerson * averageTopicsPerPaper;
        int nTopics = (int) (averageTopicsPerPerson
                / (1 - Math.pow(1 - probTopicsInCommon, 1.0 / averageTopicsPerPerson)));

        rooms = new Room[totalRooms];
        for (int i = 0; i < totalRooms; i++) {
            rooms[i] = new Room(i);
        }

        topics = new Topic[nTopics];
        for (int i = 0; i < nTopics; i++) {
            topics[i] = new Topic(i);
        }

        int totalPapers = 0;
        int totalTopics = 0;
        int totalKnown = 0;

        long startTime = System.currentTimeMillis();
        people = new Person[totalPeople];
        for (int i = 0; i < totalPeople; i++) {
            Person person = new Person(i, startTime);
            people[i] = person;

            // every person has at least one paper and at most MAX_PAPERS
            int nPersonPapers = 1 + random.nextInt(MAX_PAPERS_PER_PERSON);
            for (int j = 0; j < nPersonPapers; j++) {
                Paper paper = new Paper(totalPapers++);
                person.papers.add(paper);

                // each paper has at least one topic and at most MAX_TOPICS
                int nPaperTopics = 1 + random.nextInt(MAX_TOPICS_PER_PAPER);
                for (int k = 0; k < nPaperTopics; k++) {
                    Topic topic = topics[random.nextInt(nTopics)];
                    paper.topics.add(topic);
                    totalTopics++;
                }
            }

            // initial, uniform distribution of people over rooms
            person.moveTo(randomRoom(), startTime);
        }
        for (Person person : people) {
            int nKnown = 1 + random.nextInt(maxPeopleKnown);
            for (int j = 0; j < nKnown; j++) {
                Person other = people[random.nextInt(totalPeople)];
                person.known.add(other);
            }
            totalKnown += nKnown;
        }

        System.out.println("total people: " + totalPeople);
        System.out.println("total rooms: " + totalRooms);
        System.out.println("total papers: " + totalPapers);
        System.out.println("total topics: " + nTopics);
        System.out.println("maximum people known: " + maxPeopleKnown);
        System.out.println("average papers per person (projected): " + averagePapersPerPerson);
        System.out.println("average papers per person (actual): " + (totalPapers / (1.0 * totalPeople)));
        System.out.println("average topics per paper (projected): " + averageTopicsPerPaper);
        System.out.println("average topics per paper (actual): " + (totalTopics / (1.0 * totalPapers)));
        System.out.println("average topics per person (projected): " + averageTopicsPerPerson);
        System.out.println("average topics per person (actual): " + (totalTopics / (1.0 * totalPeople)));
        System.out.println("average people known (projected): " + averagePeopleKnown);
        System.out.println("average people known (actual): " + (totalKnown / (1.0 * totalPeople)));
        System.out.println("probability of topics in common: " + probTopicsInCommon);

        // add static metadata
        String siocNs = "http://rdfs.org/sioc/ns#";
        URI siocTopic = vf.createURI(siocNs + "topic");
        for (Person person : people) {
            queryEngine.addStatements(0, vf.createStatement(person.uri, RDF.TYPE, FOAF.PERSON));
            for (Paper paper : person.papers) {
                queryEngine.addStatements(0,
                        vf.createStatement(paper.uri, RDF.TYPE, FOAF.DOCUMENT),
                        vf.createStatement(paper.uri, FOAF.MAKER, person.uri));
                // note: we assume no papers in common
                for (Topic topic : paper.topics) {
                    queryEngine.addStatements(0,
                            vf.createStatement(topic.uri, RDF.TYPE, OWL.THING),
                            vf.createStatement(paper.uri, siocTopic, topic.uri));
                }
            }

            // each person knows at least one other person and at most MAX_PEOPLE_KNOWN
            for (Person other : person.known) {
                queryEngine.addStatements(0, vf.createStatement(person.uri, FOAF.KNOWS, other.uri));
            }
        }

        ScheduledExecutorService service = Executors.newSingleThreadScheduledExecutor();

        if (0 < timeLimitSeconds) {
            service.schedule(new Runnable() {
                @Override
                public void run() {
                    logger.info("quitting simulation after configured time limit of " + timeLimitSeconds + "s");
                    System.exit(0);
                }
            }, timeLimitSeconds, TimeUnit.SECONDS);
        }

        /*
        service.scheduleWithFixedDelay(new Runnable() {
            public void run() {
                System.out.println("queryEngine: " + queryEngine);
                System.out.println("break here");
            }
        }, initialDelay, 60, TimeUnit.SECONDS);
        //*/

        startTime = System.currentTimeMillis();

        for (int i = 0; i < totalThreads; i++) {
            int fromRoom = i * (totalRooms / totalThreads);
            int toRoom = i == totalThreads - 1 ? totalRooms - 1 : (i + 1) * (totalRooms / totalThreads) - 1;
            Simulation sim = new Simulation(i, startTime, totalPeople, fromRoom, toRoom);
            new Thread(sim).start();
        }
    }

    private synchronized int getCount(final ThreadLocal<Integer> counter) {
        Integer i = counter.get();
        return null == i ? 0 : i;
    }

    private synchronized void increment(final ThreadLocal<Integer> counter) {
        Integer i = counter.get();
        counter.set(null == i ? 1 : i + 1);
    }

    private synchronized void findPulseLatency(final long now) {
        Long then = timeOfPulseTrigger.get();
        if (null != then) {
            long latency = now - then;
            timeOfPulseTrigger.set(null);
            addToSample(pulseLatency, latency);
            //System.out.println("pulse latency = " + latency + "ms");
        }
    }

    private synchronized void findTopicsHandshakeLatency(final long now) {
        Long then = timeOfTopicsHandshakeTrigger.get();
        if (null != then) {
            long latency = now - then;
            timeOfTopicsHandshakeTrigger.set(null);
            addToSample(topicsHandshakeLatency, latency);
            //System.out.println("topics-handshake latency = " + latency + "ms");
        }
    }

    private synchronized void findFriendsHandshakeLatency(final long now) {
        Long then = timeOfFriendsHandshakeTrigger.get();
        if (null != then) {
            long latency = now - then;
            timeOfFriendsHandshakeTrigger.set(null);
            addToSample(friendsHandshakeLatency, latency);
            //System.out.println("friends-handshake latency = " + latency + "ms");
        }
    }

    private class Simulation implements Runnable {
        private final int index;
        private final long startTime;
        private final int totalPeople;
        private final int fromRoom, toRoom;

        private Simulation(int index, long startTime, int totalPeople, int fromRoom, int toRoom) {
            logger.info("creating simulation #" + index + " [" + fromRoom + ", " + toRoom + "]");
            this.index = index;
            this.startTime = startTime;
            this.totalPeople = totalPeople;
            this.fromRoom = fromRoom;
            this.toRoom = toRoom;
        }

        @Override
        public void run() {
            logger.info("running simulation #" + index);
            long lastTimeStep = startTime;
            long lastReport = startTime;

            pulseLatency.set(new LinkedList<Long>());
            friendsHandshakeLatency.set(new LinkedList<Long>());
            topicsHandshakeLatency.set(new LinkedList<Long>());

            try {
                // loop until the thread is killed after a pre-configured time limit
                while (true) {
                    //int totalPeople = 0;
                    long now = System.currentTimeMillis();
                    long elapsed = now - lastTimeStep;

                    // prevent cycles from becoming arbitrarily short and busy
                    if (elapsed < 1000) {
                        Thread.sleep(1000 - elapsed);
                        now = System.currentTimeMillis();
                    }

                    lastTimeStep = now;

                    long moveTime = 0, shakeTime = 0;
                    for (int i = fromRoom; i <= toRoom; i++) {
                        Room room = rooms[i];
                        //System.out.println("room #" + i + " has " + room.people.size() + " people");

                        // note: move first, as this updates presence
                        long before = System.currentTimeMillis(), after;
                        for (Person person : room.people) {
                            try {
                                person.considerMoving(now);
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }
                        after = System.currentTimeMillis();
                        moveTime += (after - before);

                        before = after;
                        for (Person person : room.people) {
                            //totalPeople++;
                            try {
                                person.considerShakingHands(now);
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        }
                        after = System.currentTimeMillis();
                        shakeTime += (after - before);
                    }

                    long after = System.currentTimeMillis();
                    long time = after - now;

                    // output detailed stats only so often
                    if (after - lastReport >= 5000L) {
                        lastReport = after;

                        synchronized (printMutex) {
                            System.out.println("thread #" + index + " cycle from " + now + " to " + after + " took " + time + "ms");
                            System.out.println("spent at most " + moveTime + "ms on moves and " + shakeTime + "ms on shakes");
                            //System.out.println("total people in thread #" + index + ": " + totalPeople);
                            if (time > CYCLE_LENGTH_WARN_THRESHOLD) {
                                logger.warning("long cycle: " + time + "ms");
                            }

                            System.out.println("moves this cycle: " + countOfMoves.get());
                            System.out.println("pulses this cycle: " + countOfPulses.get());
                            System.out.println("received pulses this cycle: " + countOfReceivedPulses.get());
                            System.out.println("true received pulses this cycle: " + countOfTrueReceivedPulses.get());
                            System.out.println("handshakes this cycle: " + countOfHandshakes.get());
                            System.out.println("received common-knows handshakes this cycle: " + countOfReceivedHandshakesWithCommonKnows.get());
                            System.out.println("received common-topics handshakes this cycle: " + countOfReceivedHandshakesWithCommonTopics.get());

                            // print latency statistics
                            System.out.println(formatSample("pulses", pulseLatency.get()));
                            System.out.println(formatSample("'friends' handshakes", friendsHandshakeLatency.get()));
                            System.out.println(formatSample("'topics' handshakes", topicsHandshakeLatency.get()));
                            //*
                            pulseLatency.get().clear();
                            friendsHandshakeLatency.get().clear();
                            topicsHandshakeLatency.get().clear();
                            //*/

                            /*
                            double movesPerSecond = countOfMoves.get() * 1000.0 / time;
                            double halfshakesPerSecond = countOfPulses.get() * 1000.0 / time;
                            double shakesPerSecond = countOfHandshakes.get() * 1000.0 / time;
                            double receivedHalfshakesPerSecond = (countOfReceivedPulses.get() / 2) * 1000.0 / time;
                            double receivedHandshakesWithCommonTopicsPerSecond
                                    = (countOfReceivedHandshakesWithCommonTopics.get() / 2) * 1000.0 / time;
                            double receivedHandshakesWithCommonKnowsPerSecond
                                    = (countOfReceivedHandshakesWithCommonKnows.get() / 2) * 1000.0 / time;
                            double movePeriod = totalPeople * time / (countOfMoves.get() * 1000.0);
                            double halfshakePeriod = totalPeople * time / (countOfPulses.get() * 1000.0);
                            double shakePeriod = totalPeople * time / (countOfHandshakes.get() * 1000.0);
                            double receivedHalfshakePeriod = totalPeople * time / ((countOfReceivedPulses.get() / 2) * 1000.0);
                            double receivedTruePositiveHalfshakePeriod = totalPeople * time
                                    / (countOfTrueReceivedPulses.get() * 1000.0);
                            double receivedHandshakesWithCommonTopicsPeriod = totalPeople * time
                                    / ((countOfReceivedHandshakesWithCommonTopics.get() / 2) * 1000.0);
                            double receivedHandshakesWithCommonKnowsPeriod = totalPeople * time
                                    / ((countOfReceivedHandshakesWithCommonKnows.get() / 2) * 1000.0);

                            System.out.println("average moves per second: " + movesPerSecond);
                            System.out.println("average half-shakes per second: " + halfshakesPerSecond);
                            System.out.println("average shakes per second: " + shakesPerSecond);
                            System.out.println("average received half-shakes per second: " + receivedHalfshakesPerSecond);
                            System.out.println("average received handshakes with common topics per second: "
                                    + receivedHandshakesWithCommonTopicsPerSecond);
                            System.out.println("average received handshakes with common knows per second: "
                                    + receivedHandshakesWithCommonKnowsPerSecond);
                            System.out.println("average seconds between moves, per person: " + movePeriod);
                            System.out.println("average seconds between half-shakes, per person: " + halfshakePeriod);
                            System.out.println("average seconds between shakes, per person: " + shakePeriod);
                            System.out.println("average seconds between received half-shake pairs, per person: "
                                    + receivedHalfshakePeriod);
                            System.out.println("average seconds between received true positive half-shake pairs, per person: "
                                    + receivedTruePositiveHalfshakePeriod);
                            System.out.println("average seconds between handshakes with common topics, per person: "
                                    + receivedHandshakesWithCommonTopicsPeriod);
                            System.out.println("average seconds between handshakes with common knows, per person: "
                                    + receivedHandshakesWithCommonKnowsPeriod);
                                    */
                        }
                    }

                    countOfMoves.set(0);
                    countOfPulses.set(0);
                    countOfHandshakes.set(0);
                    countOfReceivedPulses.set(0);
                    countOfTrueReceivedPulses.set(0);
                    countOfReceivedHandshakesWithCommonKnows.set(0);
                    countOfReceivedHandshakesWithCommonTopics.set(0);
                }
            } catch (Throwable t) {
                logger.log(Level.SEVERE, "simulation thread died with error", t);
            }
        }
    }

    private String formatSample(String name, List<Long> sample) {
        double mean = Stats.findMean(sample);
        double sd = Stats.findStandardDeviation(sample, mean);
        return name + ": mean=" + mean + ", sd=" + sd;
    }

    // star topology minimizes repeat handshakes
    private Room randomRoom() {
        return rooms[random.nextInt(rooms.length)];
    }

    private String handshakeKey(final long now, final Person person1, final Person person2) {
        int id1, id2;
        if (person1.id < person2.id) {
            id1 = person1.id;
            id2 = person2.id;
        } else {
            id1 = person2.id;
            id2 = person1.id;
        }

        return "" + 1000L * (now / 1000L) + ":" + id1 + ":" + id2;
    }

    private void handshake(final Person person1, final Person person2) throws IOException {
        increment(countOfHandshakes);

        if (verbose) {
            System.out.println(person1 + " SHAKES with " + person2);
        }

        long now = System.currentTimeMillis();

        Dataset d = Activities.datasetForHandshakeInteraction(now, person1.uri, person2.uri);

        timeOfLastHandshake.set(now);
        timeOfFriendsHandshakeTrigger.set(now);
        timeOfTopicsHandshakeTrigger.set(now);

        queryEngine.addStatements(HANDSHAKE_TTL, toArray(d));
    }

    private void halfHandshakes(final Person person1, final Person person2) throws IOException {
        increment(countOfPulses);

        long now = System.currentTimeMillis();

        String key = handshakeKey(now, person1, person2);
        if (handshakesInProgress.contains(key)) {
            logger.warning("adding duplicate key: " + key);
        }
        handshakesInProgress.add(key);

        if (verbose) {
            System.out.println(person1 + " half-shakes with " + person2 + " --> " + handshakesInProgress.size() + " " + key);
        }

        // note: the fact that the gestures occur at *exactly* the same moment is unimportant
        Dataset d1 = Activities.datasetForHandshakePulse(now, person1.uri);
        Dataset d2 = Activities.datasetForHandshakePulse(now, person2.uri);

        /*
        try {
            RDFWriter w = Rio.createWriter(RDFFormat.NTRIPLES, System.out);
            w.startRDF();
            for (Statement s : d1.getStatements()) {
                w.handleStatement(s);
            }
            w.endRDF();
        } catch (Throwable t) {
            logger.severe(t.getMessage());
            System.exit(1);
        }
        System.exit(0);
        */

        now = System.currentTimeMillis();
        timeOfLastPulse.set(now);
        timeOfPulseTrigger.set(now);

        queryEngine.addStatements(person1.halfHandshakeTtl, toArray(d1));
        queryEngine.addStatements(person2.halfHandshakeTtl, toArray(d2));
    }

    private void presence(final Person person, final Room room) throws IOException {
        Statement st = vf.createStatement(
                person.uri, vf.createURI(ExtendoActivityOntology.NAMESPACE + "locatedAt"), room.uri);
        queryEngine.addStatements(presenceTtl, st);
    }

    private Statement[] toArray(final Dataset d) {
        Collection<Statement> c = d.getStatements();
        Statement[] a = new Statement[c.size()];
        return c.toArray(a);
    }

    private boolean doTransition(final long averageDwellTime, final long cycleLength) {
        double probMove = 1.0 - Math.pow(0.5, cycleLength * Math.sqrt(2) / averageDwellTime);
        return random.nextDouble() < probMove;
    }

    private <T> Set<T> newConcurrentSet() {
        return Collections.newSetFromMap(new ConcurrentHashMap<T, Boolean>());
    }

    private abstract class Thing {
        protected final int id;
        protected final URI uri;

        private Thing(final int id) {
            this.id = id;
            this.uri = new URIImpl(EX + type() + id);
        }

        protected abstract String type();

        @Override
        public String toString() {
            return type() + id;
        }

        @Override
        public boolean equals(final Object other) {
            return other instanceof Thing && ((Thing) other).uri.equals(uri);
        }

        @Override
        public int hashCode() {
            return uri.hashCode();
        }
    }

    private class Person extends Thing {
        private final int halfHandshakeTtl;
        private final Collection<Paper> papers = new LinkedList<Paper>();
        private final Collection<Person> known = new LinkedList<Person>();

        private Long
                timeOfLastPresence,
                timeOfLastMove,
                timeOfLastHandshake;
        private long
                timeLastConsideredMove,
                timeLastConsideredHandshake;

        private Room currentRoom;

        private Person(final int id,
                       final long startTime) {
            super(id);

            // note: avoids 0 as a value of TTL
            this.halfHandshakeTtl = 1 + random.nextInt(MAX_HANDSHAKE_PULSE_TTL);

            this.timeOfLastPresence = 0L;
            this.timeLastConsideredMove = startTime;
            this.timeLastConsideredHandshake = startTime;
        }

        @Override
        protected String type() {
            return "person";
        }

        public void moveTo(final Room room, long now) throws IOException {
            //if (verbose) {
            System.out.println(this + " is moving to " + room
                    + (null == timeOfLastMove ? "" : (" after " + (now - timeOfLastMove) / 1000) + "s dwell time"));
            //}

            timeOfLastMove = now;

            if (null != currentRoom) {
                currentRoom.people.remove(this);
            }

            currentRoom = room;
            currentRoom.people.add(this);
        }

        public void considerMoving(long now) throws IOException {
            long elapsed = now - timeLastConsideredMove;
            timeLastConsideredMove = now;

            boolean refreshPresence = false;
            if (doTransition(averageMillisecondsBetweenMoves, elapsed)) {
                Room newRoom;
                do {
                    newRoom = randomRoom();
                } while (newRoom == currentRoom);

                increment(countOfMoves);
                moveTo(newRoom, now);
                refreshPresence = true;
            } else if (now - timeOfLastPresence >= averageMillisecondsBetweenMoves) {
                refreshPresence = true;
            }

            // refresh the person's current position when they move or when TTL has expired
            if (refreshPresence) {
                timeOfLastPresence = now;
                presence(this, currentRoom);
            }
        }

        public void considerShakingHands(long now) throws IOException {
            long elapsed = now - timeLastConsideredHandshake;
            timeLastConsideredHandshake = now;

            if (doTransition(averageMillisecondsBetweenHandshakes, elapsed)) {
                // choose a single person in the newly entered space to shake hands with
                if (currentRoom.people.size() > 1) {
                    for (Person other : currentRoom.people) {
                        // don't shake hands with self
                        if (other.equals(this)) {
                            continue;
                        }

                        //if (verbose) {
                        System.out.println(this + " is shaking hands"
                                + (null == timeOfLastPulse.get() ? "" : (" after " + (now - timeOfLastPulse.get()) / 1000) + "s idle time"));
                        //}

                        halfHandshakes(this, other);
                        break;
                    }
                }
            }
        }
    }

    private class Room extends Thing {
        // note: needs to be thread-safe so the simulator threads can iterate over people while moves are in progress
        private final Set<Person> people = newConcurrentSet();

        private Room(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "room";
        }
    }

    private class Paper extends Thing {
        private final Collection<Topic> topics = new LinkedList<Topic>();

        public Paper(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "paper";
        }
    }

    private class Topic extends Thing {
        public Topic(final int id) {
            super(id);
        }

        @Override
        protected String type() {
            return "topic";
        }
    }

    /*
        public SesameStreamEvaluation(final boolean verbose,
                                  final int totalThreads,
                                  final int totalPeople,
                                  final int totalRooms,
                                  final Set<String> queries,
                                  final int moveTime,
                                  final int shakeTIme,
                                  final int timeLimitSeconds,
                                  final double probTopicsInCommon)
     */
    public static void main(final String[] args) throws Exception {
        /*
        Set<String> qs = new HashSet<String>();
        qs.add("topics");
        new SesameStreamEvaluation(false, 1, 400, 5, qs, 600, 300, 0, 0.5);
        if (true) return;
        //*/

        try {
            Options options = new Options();

            Option threadsOpt = new Option("t", "threads", true, "number of worker threads (default: 4)");
            threadsOpt.setArgName("THREADS");
            threadsOpt.setRequired(false);
            options.addOption(threadsOpt);

            Option peopleOpt = new Option("p", "people", true, "number of people (default: 100)");
            peopleOpt.setArgName("PEOPLE");
            peopleOpt.setRequired(false);
            options.addOption(peopleOpt);

            Option roomsOpt = new Option("r", "rooms", true, "number of rooms (default: 10)");
            roomsOpt.setArgName("ROOMS");
            roomsOpt.setRequired(false);
            options.addOption(roomsOpt);

            Option queriesOpt = new Option("q", "queries", true, "queries (default: topics)");
            queriesOpt.setArgName("QUERIES");
            queriesOpt.setRequired(false);
            options.addOption(queriesOpt);

            Option limitOpt = new Option("l", "limit", true, "time limit in seconds");
            limitOpt.setArgName("SECONDS");
            limitOpt.setRequired(false);
            options.addOption(limitOpt);

            Option moveTimeOpt = new Option("m", "interMoveTime", true,
                    "average time between moves, in seconds (default: 300)");
            moveTimeOpt.setArgName("SECONDS");
            moveTimeOpt.setRequired(false);
            options.addOption(moveTimeOpt);

            Option shakeTimeOpt = new Option("h", "interHandshakeTime", true,
                    "average time between handshakes, in seconds (default: 180)");
            shakeTimeOpt.setArgName("SECONDS");
            shakeTimeOpt.setRequired(false);
            options.addOption(shakeTimeOpt);

            Option pTopicOpt = new Option("", "pTopic", true, "probability of common topics (default: 0.8)");
            pTopicOpt.setArgName("PROBABILITY");
            pTopicOpt.setRequired(false);
            options.addOption(pTopicOpt);

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

            int nThreads = Integer.valueOf(cmd.getOptionValue(threadsOpt.getOpt(), "4"));
            int nPeople = Integer.valueOf(cmd.getOptionValue(peopleOpt.getOpt(), "100"));
            int nRooms = Integer.valueOf(cmd.getOptionValue(roomsOpt.getOpt(), "8"));
            String queriesStr = cmd.getOptionValue(queriesOpt.getOpt(), "topics");
            int timeLimitSeconds = Integer.valueOf(cmd.getOptionValue(limitOpt.getOpt(), "0"));
            int moveTime = Integer.valueOf(cmd.getOptionValue(moveTimeOpt.getOpt(), "300"));
            int shakeTime = Integer.valueOf(cmd.getOptionValue(shakeTimeOpt.getOpt(), "180"));
            double pTopic = Double.valueOf(cmd.getOptionValue(pTopicOpt.getOpt(), "0.8"));
            boolean verbose = cmd.hasOption(verboseOpt.getOpt());

            Set<String> queries = new HashSet<String>();
            for (String q : queriesStr.split(";")) {
                String query = q.trim();
                if (0 == query.length()) {
                    printUsageAndExit(options);
                }
                queries.add(query);
            }

            new SesameStreamEvaluation(verbose, nThreads, nPeople, nRooms, queries, moveTime, shakeTime, timeLimitSeconds, pTopic);
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void printUsageAndExit(final Options options) {
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("sesamestream-eval", options);
        //System.out.println("options: " + options.toString());
        //System.err.println("see source for usage");
        System.exit(1);
    }
}
