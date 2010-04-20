package edu.rpi.tw.patadata;

import edu.rpi.tw.patadata.ranking.ApproxIntersection;
import edu.rpi.tw.patadata.ranking.NormalizedApproxVector;
import edu.rpi.tw.patadata.ranking.WeightedValue;
import edu.rpi.tw.patadata.ranking.WeightedVector;
import edu.rpi.tw.patadata.ranking.WeightedVectorApproximation;
import info.aduna.iteration.CloseableIteration;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.impl.LiteralImpl;
import org.openrdf.model.impl.URIImpl;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFParseException;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;
import org.openrdf.sail.nativerdf.NativeStore;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

/**
 * User: josh
 * Date: Apr 16, 2010
 * Time: 4:50:03 PM
 */
public class Patadata {
    private static final URI prometheus = new URIImpl("http://dbpedia.org/resource/Prometheus");
    private static final URI piano = new URIImpl("http://dbpedia.org/resource/Piano");
    private static final URI rock = new URIImpl("http://dbpedia.org/resource/Rock_%28geology%29");
    private static final URI ring = new URIImpl("http://dbpedia.org/resource/Ring");
    private static final URI mountain = new URIImpl("http://dbpedia.org/resource/Mountain");
    private static final URI monkey = new URIImpl("http://dbpedia.org/resource/Monkey");
    private static final URI hamlet = new URIImpl("http://dbpedia.org/resource/Hamlet");
    private static final URI typewriter = new URIImpl("http://dbpedia.org/resource/Typewriter");
    private static final URI music = new URIImpl("http://dbpedia.org/resource/Music");

    public static final URI
            ADMIN_GRAPH = new URIImpl("http://example.org/adminGraph"),
            INDEX = new URIImpl("http://example.org/index");

    private static final String BASE_URI = "http://example.org/bogusBaseURI/";
    private final Sail sail;

    public Patadata() throws Exception {
        /*
        GraphDatabaseService graphDb = new EmbeddedGraphDatabase(
                "/Users/josh/projects/school/patadata/data");
        IndexService indexService = new LuceneIndexService(graphDb);
        RdfStore rdfStore = new VerboseQuadStore(graphDb, indexService);
        sail = new GraphDatabaseSail(graphDb, rdfStore);
        */

        sail = new NativeStore(new File("/Users/josh/projects/school/patadata/data"));

        sail.initialize();

        try {
            //loadDBPedia();
            //enumerateResources();
            //showTripleStoreInfo();

            //testSingleSeed();
            //testRandomResources();
            //testSimpleSuperposition();
            testIntersection();
            //testBogoPageRank();
            //testSyzygySearch();
        } finally {
            sail.shutDown();
        }
    }

    private void loadDBPedia() throws RepositoryException, IOException, RDFParseException {
        // 119,074,656 triples
//        File wikilinks = new File("/Users/josh/datasets/dbpedia/page_links_en.nt");

        // 20,000,000 triples
        // NativeStore estimates 16,825,463 triples after loading
        // Exactly 408,422 unique subject resources
        File wikilinks = new File("/Users/josh/datasets/dbpedia/page_links_en_first20million.nt");

        Repository repo = new SailRepository(sail);
        final RepositoryConnection rc = repo.getConnection();

        try {
            rc.clear();
            rc.add(wikilinks, BASE_URI, RDFFormat.NTRIPLES);
            rc.commit();
        } catch (Throwable t) {
            t.printStackTrace();
        } finally {
            rc.close();
        }
    }

    private void enumerateResources() throws SailException {
        Set<Resource> subjects = new HashSet<Resource>();

        SailConnection sc = sail.getConnection();
        try {
            System.out.println("clearing admin graph");
            sc.clear(ADMIN_GRAPH);
            sc.commit();
            System.out.println("done");

            CloseableIteration<? extends Statement, SailException> iter
                    = sc.getStatements(null, null, null, false);
            try {
                System.out.println("cycling through statements");
                int count = 1;
                int queryChunkSize = 1000000;

                while (iter.hasNext()) {
                    Resource subject = iter.next().getSubject();
                    subjects.add(subject);
                    count++;
                    if (0 == count % queryChunkSize) {
                        System.out.println("cycled through " + count + " statements");
                    }
                }
            } finally {
                iter.close();
            }
            System.out.println("done cycling through statements");
        } finally {
            sc.close();
        }

        sc = sail.getConnection();
        try {
            System.out.println("adding enumeration index values");
            int chunkSize = 10000;
            int count = 0;
            for (Resource subject : subjects) {
                sc.addStatement(subject, INDEX, new LiteralImpl("" + count), ADMIN_GRAPH);
                count++;
                if (0 == count % chunkSize) {
                    System.out.println("indexed " + count + " resources");
                    sc.commit();
                }
            }
            sc.commit();
            System.out.println("done indexing resources (" + count + " total)");
        } finally {
            sc.close();
        }
    }

    private void testSyzygySearch() throws SailException, PataException {
        Resource r1 = prometheus;
        Resource r2 = piano;

        SailConnection sc = sail.getConnection();
        try {
            System.out.println("before PageRank");

            for (int i = 0; i < 1; i++) {
                long before = System.currentTimeMillis();

                System.out.println("r1: " + r1);
                System.out.println("r2: " + r2);

                //WeightedVector<Resource> result = spread(sc, 1000, piano, prometheus);
                WeightedVector<Resource> result = spread(sc, 1000, r1, r2);
                result.normalizeAsDist();
                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
            }

            //System.out.println("computing PageRank");
            //WeightedVector<Resource> pageRank = computeBogoPageRank(sc);

            System.out.println("after PageRank");

            for (int i = 0; i < 1; i++) {
                long before = System.currentTimeMillis();

                System.out.println("r1: " + r1);
                System.out.println("r2: " + r2);

                //WeightedVector<Resource> result = spread(sc, 1000, piano, prometheus);
                WeightedVector<Resource> result = spread(sc, 1000, r1, r2);

                //result = result.subtract(pageRank).positiveClip();

                result.normalizeAsDist();
                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
            }
        } finally {
            sc.close();
        }
    }

    private void testSyzygySearchOld() throws SailException, PataException {
        Resource r1 = prometheus;
        Resource r2 = piano;

        SailConnection sc = sail.getConnection();
        try {
            System.out.println("before PageRank");

            for (int i = 0; i < 1; i++) {
                long before = System.currentTimeMillis();

                System.out.println("r1: " + r1);
                System.out.println("r2: " + r2);

                //WeightedVector<Resource> result = spread(sc, 1000, piano, prometheus);
                WeightedVector<Resource> result = spread(sc, 10000, r1, r2);
                result.normalizeAsDist();
                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
            }

            System.out.println("computing PageRank");

            WeightedVector<Resource> pageRank = computeBogoPageRank(sc);

            System.out.println("after PageRank");

            for (int i = 0; i < 1; i++) {
                long before = System.currentTimeMillis();

                System.out.println("r1: " + r1);
                System.out.println("r2: " + r2);

                //WeightedVector<Resource> result = spread(sc, 1000, piano, prometheus);
                WeightedVector<Resource> result = spread(sc, 10000, r1, r2);

                result = result.subtract(pageRank).positiveClip();

                result.normalizeAsDist();
                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
            }
        } finally {
            sc.close();
        }
    }

    private void testSingleSeed() throws Exception {
        SailConnection sc = sail.getConnection();
        try {
            for (int i = 0; i < 1; i++) {
                long before = System.currentTimeMillis();
                WeightedVector<Resource> result = spread(sc, 300, prometheus);
//                WeightedVector<Resource> result = spread(sc, 300, piano);
                result.normalizeAsDist();
                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");

                //*/
                System.out.println("now subtracting PageRank");
                before = System.currentTimeMillis();
                WeightedVector<Resource> pageRank = computeBogoPageRank(sc);
                result = result.subtract(pageRank).positiveClip();
                result.normalizeAsDist();
                showResults(result);
                after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
                //*/
            }
        } finally {
            sc.close();
        }
    }

    private void testBogoPageRank() throws SailException, PataException {
        SailConnection sc = sail.getConnection();
        try {
            for (int i = 0; i < 3; i++) {
                long before = System.currentTimeMillis();
                WeightedVectorApproximation<Resource, PataException> v
                        = new BogoPageRankVector(sc);

                // 300 not enough
                // 1000 still not enough
                // 10,000/100 is pretty consistent
                v.compute(10000);

                WeightedVector<Resource> result = v.currentResult();

                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
            }
        } finally {
            sc.close();
        }
    }

    private WeightedVector<Resource> computeBogoPageRank(final SailConnection sc) throws SailException, PataException {
        long before = System.currentTimeMillis();
        WeightedVectorApproximation<Resource, PataException> v
                = new BogoPageRankVector(sc);

        v.compute(10000);

        WeightedVector<Resource> result = v.currentResult();

        showResults(result);
        long after = System.currentTimeMillis();
        System.out.println("completed in " + (after - before) + "ms");
        return result;
    }

    private void testSimpleSuperposition() throws Exception {
        SailConnection sc = sail.getConnection();
        try {
            for (int i = 0; i < 3; i++) {
                long before = System.currentTimeMillis();

                Resource r1 = piano;
                Resource r2 = prometheus;
                //Resource r1 = TraverserTools.randomResource(sc);
                //Resource r2 = TraverserTools.randomResource(sc);
                System.out.println("r1: " + r1);
                System.out.println("r2: " + r2);

                //WeightedVector<Resource> result = spread(sc, 1000, piano, prometheus);
                WeightedVector<Resource> result = spread(sc, 1000, r1, r2);
                result.normalizeAsDist();
                showResults(result);
                long after = System.currentTimeMillis();
                System.out.println("completed in " + (after - before) + "ms");
            }
        } finally {
            sc.close();
        }
    }

    private void testIntersection() throws Exception {
        SailConnection sc = sail.getConnection();
        try {
            long before = System.currentTimeMillis();
//            WeightedVector<Resource> intersection = findSpreadIntersection(sc, 19, monkey, hamlet);
//            WeightedVector<Resource> intersection = findSpreadIntersection(sc, 10, monkey, typewriter);
            WeightedVector<Resource> result = findSpreadIntersection(sc, 1000, prometheus, rock);
            long after = System.currentTimeMillis();

            System.out.println("completed in " + (after - before) + "ms");
            showResults(result);


            //*/
            System.out.println("now subtracting PageRank");
            before = System.currentTimeMillis();
            WeightedVector<Resource> pageRank = computeBogoPageRank(sc);
            result = result.subtract(pageRank).positiveClip();
            result.normalizeAsDist();
            showResults(result);
            after = System.currentTimeMillis();
            System.out.println("completed in " + (after - before) + "ms");
            //*/
        } finally {
            sc.close();
        }
    }

    private WeightedVector<Resource> findSpreadIntersection(final SailConnection sc,
                                                            final int cycles,
                                                            final Resource... seeds) throws PataException {
        WeightedVectorApproximation<Resource, PataException> p = dbpediaSpreadIntersection(sc, seeds);
        p.compute(cycles);
        return p.currentResult();
    }

    public static WeightedVectorApproximation<Resource, PataException> dbpediaSpreadIntersection(
            final SailConnection sc,
            final Resource... seeds) {
        WeightedVectorApproximation<Resource, PataException>[] spreadVectors
                = new WeightedVectorApproximation[seeds.length];
        for (int i = 0; i < seeds.length; i++) {
            spreadVectors[i] = new DBPediaSpreadVector(sc, seeds[i]);
        }

        return new NormalizedApproxVector<Resource, PataException>(
                new ApproxIntersection<Resource, PataException>(spreadVectors));
    }

    private void testRandomResources() throws SailException, PataException {
        SailConnection sc = sail.getConnection();
        try {
            for (int i = 0; i < 1; i++) {
                Resource r = prometheus;
                System.out.println("random resource: " + r);

                Handler<Resource, PataException> ins = new Handler<Resource, PataException>() {
                    public boolean handle(Resource resource) throws PataException {
                        System.out.println("\t" + resource);
                        return true;
                    }
                };

                TraverserTools.traverseForward(sc, new KeepResourcesFilter(ins),
                        r, DBPediaSpreadVector.RELATED_RESOURCE_PREDICATES);
            }

            /*
            Resource r1 = TraverserTools.randomResource(sc);
            Resource r2 = TraverserTools.randomResource(sc);
            System.out.println("r1: " + r1);
            System.out.println("r2: " + r2);

            findSpreadIntersection(sc, 1000, r1, r2);
            */
        } finally {
            sc.close();
        }
    }

    private static final int MAX_DISPLAY_RESULTS = 20;

    private void showResults(final WeightedVector results) {
        System.out.println("vector of size " + results.size() + ":");

        WeightedValue[] a = results.toSortedArray();

        for (int i = 0; i < Math.min(MAX_DISPLAY_RESULTS, a.length); i++) {
            WeightedValue wv = a[i];
            System.out.println("\t" + (1000000 * wv.weight) + "\t" + wv.value);
        }

        if (a.length > MAX_DISPLAY_RESULTS) {
            System.out.println("\t[...]");
        }
    }

    public WeightedVector<Resource> spread(final SailConnection sc,
                                           final int flops,
                                           final Resource... seeds) throws PataException {
        DBPediaSpreadVector spreader = new DBPediaSpreadVector(sc, seeds);
        spreader.compute(flops);

        return spreader.currentResult();
    }

    private void showTripleStoreInfo() throws SailException {
        SailConnection sc = sail.getConnection();
        try {
            System.out.println("" + sc.size() + " triples");
        } finally {
            sc.close();
        }
    }

    public static void main(final String[] args) {
        try {
            Patadata p = new Patadata();
            //p.
        } catch (Throwable t) {
            System.out.println("failed with exception thrown:");
            t.printStackTrace();
            System.exit(1);
        }
    }
}
