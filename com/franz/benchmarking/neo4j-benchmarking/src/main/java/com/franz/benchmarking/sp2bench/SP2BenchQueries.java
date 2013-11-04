package com.franz.benchmarking.sp2bench;

import com.tinkerpop.blueprints.Vertex;
import com.tinkerpop.blueprints.impls.neo4j.Neo4jGraph;
import com.tinkerpop.blueprints.util.wrappers.id.IdGraph;
import com.tinkerpop.gremlin.groovy.Gremlin;
import com.tinkerpop.pipes.Pipe;
import com.tinkerpop.pipes.util.iterators.SingleIterator;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.kernel.impl.util.StringLogger;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SP2BenchQueries {

    private static void showQueryResult(final String query,
                                        final ExecutionEngine engine) {
        long time1 = System.currentTimeMillis();
        ExecutionResult result = engine.execute(query);
        long time2 = System.currentTimeMillis();

        String rows = "";
        for (Map<String, Object> row : result) {
            for (Map.Entry<String, Object> column : row.entrySet()) {
                rows += column.getKey() + ": " + column.getValue() + "; ";
            }
            rows += "\n";
        }
        System.out.print(rows);
        long time3 = System.currentTimeMillis();

        System.out.println("execution time: " + (time2 - time1) + "ms");
        System.out.println("iteration time: " + (time3 - time2) + "ms");
    }

    private static void timeCypherQuery(final Query query,
                                        final int iters,
                                        final int sets,
                                        final List<Long> counts,
                                        final List<Long> times) {
        for (int j = 0; j < sets; j++) {
            long time1 = System.currentTimeMillis();

            long count = query.execute(iters);

            long time2 = System.currentTimeMillis();

            counts.add(count);
            times.add(time2 - time1);
        }
    }

    private static void printTimedQuery(final Query query,
                                        final int iters,
                                        final int sets) {
        List<Long> counts = new LinkedList<Long>();
        List<Long> times = new LinkedList<Long>();

        timeCypherQuery(query, iters, sets, counts, times);

        StringBuilder sb = new StringBuilder();
        sb.append(query.getName());
        sb.append(" <- c(");
        boolean first = true;
        for (long time : times) {
            if (first) first = false;
            else sb.append(", ");

            sb.append(time);
        }
        sb.append(")");

        System.out.println(sb);

        sb = new StringBuilder();
        sb.append(query.getName());
        sb.append(".count <- c(");
        first = true;
        for (long count : counts) {
            if (first) first = false;
            else sb.append(", ");

            sb.append(count);
        }
        sb.append(")");

        System.out.println(sb);
    }

    public static void main(final String[] args) throws Exception {

        String pathToDatabase = "/tmp/neo-sp2bench";

        new SP2BenchQueries().evaluateCypher(pathToDatabase);
        //new SP2BenchQueries().evaluateGremlin(pathToDatabase);
    }

    private void evaluateCypher(final String pathToDatabase) {
        GraphDatabaseFactory factory = new GraphDatabaseFactory();
        GraphDatabaseService g = factory.newEmbeddedDatabase(pathToDatabase);
        try {
            // Return the year of publication of Journal 1 (1940).
            String q1_cypher = "START journal=node(*)\n" +
                    "MATCH journal-[:`rdf:type`]->bj, journal-[:`dc:title`]->title, journal-[:`dcterms:issued`]->year\n" +
                    "WHERE bj.__id = 'bench:Journal' AND title.__id = '\"Journal 1 (1940)\"'\n" +
                    "RETURN year.__id";
            String q1_cypher_cheat = "START title=node:node_auto_index(__id = \"\\\"Journal 1 (1940)\\\"\")\n" +
                    "MATCH journal-[:`rdf:type`]->bj, journal-[:`dc:title`]->title, journal-[:`dcterms:issued`]->year\n" +
                    "WHERE bj.__id = 'bench:Journal'\n" +
                    "RETURN year.__id";

            // Extract all inproceedings with properties
            // dc:creator, bench:booktitle, dc:title, swrc:pages, dcterms:partOf, rdfs:seeAlso, foaf:homepage
            // dcterms:issued, and optionally bench:abstract, including these properties, ordered by year.
            String q2_cypher = "START inproc=node(*)\n" +
                    "MATCH inproc-[:`rdf:type`]->bip," +
                    " inproc-[:`dc:creator`]->author," +
                    " inproc-[:`bench:booktitle`]->booktitle," +
                    " inproc-[:`dc:title`]->title," +
                    " inproc-[:`dcterms:partOf`]->proc," +
                    " inproc-[:`rdfs:seeAlso`]->ee," +
                    " inproc-[:`swrc:pages`]->page," +
                    " inproc-[:`foaf:homepage`]->url," +
                    " inproc-[:`dcterms:issued`]->yr," +
                    " inproc-[?:`bench:abstract`]->abstract\n" +
                    "WHERE bip.__id = 'bench:Inproceedings'\n" +
                    "RETURN inproc.__id, author.__id, booktitle.__id, title.__id, proc.__id, ee.__id, page.__id, url.__id, yr.__id, abstract.__id";
            String q2_cypher_cheat = "START bip=node:node_auto_index(__id = \"bench:Inproceedings\")\n" +
                    "MATCH inproc-[:`rdf:type`]->bip," +
                    " inproc-[:`dc:creator`]->author," +
                    " inproc-[:`bench:booktitle`]->booktitle," +
                    " inproc-[:`dc:title`]->title," +
                    " inproc-[:`dcterms:partOf`]->proc," +
                    " inproc-[:`rdfs:seeAlso`]->ee," +
                    " inproc-[:`swrc:pages`]->page," +
                    " inproc-[:`foaf:homepage`]->url," +
                    " inproc-[:`dcterms:issued`]->yr," +
                    " inproc-[?:`bench:abstract`]->abstract\n" +
                    "RETURN inproc.__id, author.__id, booktitle.__id, title.__id, proc.__id, ee.__id, page.__id, url.__id, yr.__id, abstract.__id";

            // Select all articles with property swrc:pages.
            String q3a_cypher = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:pages`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";

            // Select all articles with property swrc:month.
            String q3b_cypher = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:month`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";

            // Select all articles with property swrc:isbn.
            String q3c_cypher = "START article=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art, article-[:`swrc:isbn`]->value\n" +
                    "WHERE Art.__id = 'bench:Article'\n" +
                    "RETURN article.__id";

            // Select all distinct pairs of article author names for authors that have published in the same journal.
            // TODO: very poor performance (perhaps never terminating)
            String q4_cypher = "START article1=node(*), article2=node(*)\n" +
                    "MATCH article1-[:`rdf:type`]->Art," +
                    " article2-[:`rdf:type`]->Art," +
                    " article1-[:`dc:creator`]->author1," +
                    " author1-[:`foaf:name`]->name1," +
                    " article2-[:`dc:creator`]->author2," +
                    " author2-[:`foaf:name`]->name2," +
                    " article1-[:`swrc:journal`]->journal1," +
                    " article2-[:`swrc:journal`]->journal2\n" +
                    "WHERE Art.__id = 'bench:Article' AND name1.__id < name2.__id\n" +
                    "RETURN DISTINCT name1.__id, name2.__id";

            // Return the names of all persons that occur as author of at least one inproceeding and at least one article.
            // Note: queries q5 and q5a are treated as the same query, as they differ only in FILTER syntax
            String q5_cypher = "START person=node(*)\n" +
                    "MATCH article-[:`rdf:type`]->Art," +
                    " article-[:`dc:creator`]->person," +
                    " inproc-[:`rdf:type`]->Inproc," +
                    " inproc-[:`dc:creator`]->person," +
                    " person-[:`foaf:name`]->name\n" +
                    "WHERE Art.__id = 'bench:Article' AND Inproc.__id = 'bench:Inproceedings'\n" +
                    "RETURN DISTINCT person.__id, name.__id";

            ExecutionEngine engine = new ExecutionEngine(g, StringLogger.SYSTEM);
            Transaction tx = g.beginTx();

            //showQueryResult(q1, engine);

            //Query q = new CypherQuery("q1.neo", q1_cypher, engine);
            //Query q = new CypherQuery("q1.neo", q1_cypher_cheat, engine);

            //Query q = new CypherQuery("q2.neo", q2_cypher, engine);
            Query q = new CypherQuery("q2.neo", q2_cypher_cheat, engine);

            //Query q = new CypherQuery("q3a.neo", q3c_cypher, engine);
            //Query q = new CypherQuery("q3b.neo", q3c_cypher, engine);
            //Query q = new CypherQuery("q3c.neo", q3c_cypher, engine);
            //Query q = new CypherQuery("q4.neo", q4_cypher, engine);
            //Query q = new CypherQuery("q5.neo", q5_cypher, engine);

            printTimedQuery(q, 1, 10);
        } finally {
            g.shutdown();
        }
    }


    private void evaluateGremlin(final String pathToDatabase) {
        Neo4jGraph g = new Neo4jGraph(pathToDatabase);

        try {
            // TODO: necessary?
            g.getRawGraph().index().getNodeAutoIndexer().startAutoIndexingProperty(IdGraph.ID);

            IdGraph ig = new IdGraph(g, true, false);

            String q1_gremlin_root = "\"Journal 1 (1940)\"";
            String q1_gremlin = "_().in(\"dc:title\").as(\"journal\").out(\"rdf:type\").filter{it.id.equals(\"bench:Journal\")}.back(\"journal\")out(\"dcterms:issued\")";

            Query q = new GremlinQuery("q1.grm", q1_gremlin_root, q1_gremlin, ig);

            printTimedQuery(q, 1, 10);
        } finally {
            g.shutdown();
        }
    }


    private abstract class Query {
        private final String name;

        protected Query(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public abstract long execute(int iters);
    }

    private class CypherQuery extends Query {
        private final String queryString;
        private final ExecutionEngine engine;

        public CypherQuery(final String name,
                           final String queryString,
                           final ExecutionEngine engine) {
            super(name);
            this.engine = engine;
            this.queryString = queryString;
        }

        public long execute(final int iters) {
            // Note: if there are multiple iterations, only the last count is used.  All counts shoud be identical.
            // If for some reason there are no iterations, a count of 0 is used
            long count = 0;

            for (int i = 0; i < iters; i++) {
                count = 0;
                ExecutionResult result = engine.execute(queryString);

                for (Map<String, Object> row : result) {
                    count++;
                }
            }

            return count;
        }
    }

    private class GremlinQuery extends Query {
        private final Pipe pipe;
        private final IdGraph ig;
        private final String rootId;

        private GremlinQuery(final String name,
                             final String rootId,
                             final String queryString,
                             final IdGraph ig) {
            super(name);
            this.ig = ig;
            this.rootId = rootId;

            pipe = Gremlin.compile(queryString);
        }

        public long execute(final int iters) {
            long count = 0;

            for (int i = 0; i < iters; i++) {
                count = 0;

                Vertex root = ig.getVertex(rootId);
                if (null == root) {
                    throw new IllegalStateException("root vertex not found: " + rootId);
                }

                pipe.setStarts(new SingleIterator<Vertex>(root));
                for (Object r : pipe) {
                    count++;
                }
            }

            return count;
        }
    }
}
