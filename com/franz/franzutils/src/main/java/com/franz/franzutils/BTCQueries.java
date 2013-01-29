package com.franz.franzutils;

import com.knowledgereefsystems.agsail.AllegroSail;
import info.aduna.iteration.CloseableIteration;
import org.openrdf.query.BindingSet;
import org.openrdf.query.MalformedQueryException;
import org.openrdf.query.QueryEvaluationException;
import org.openrdf.query.impl.MapBindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.sparql.SPARQLParser;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.io.File;

/**
 * Author: josh
 * Date: Jun 1, 2009
 * Time: 4:55:10 PM
 */
public class BTCQueries {
    private static ParsedQuery parseQuery(final String queryString) throws MalformedQueryException {
        SPARQLParser parser = new SPARQLParser();

        // FIXME
        String baseURI = "http://example.org/bogusBaseURI/";

        return parser.parseQuery(queryString, baseURI);
    }

    private static long evaluateAndCount(final ParsedQuery query,
                                         final Sail sail) throws SailException, QueryEvaluationException {
        SailConnection sc = sail.getConnection();
        try {
            BindingSet bs = new MapBindingSet();
            boolean includeInferred = false;
            CloseableIteration<? extends BindingSet, QueryEvaluationException> results
                    = sc.evaluate(query.getTupleExpr(), query.getDataset(), bs, includeInferred);
            try {
                long count = 0;

                while (results.hasNext()) {
                    count++;
                    results.next();
                }

                return count;
            } finally {
                results.close();
            }
        } finally {
            sc.close();
        }
    }

    private static Sail createAllegroSail(final String host,
                                          final int port,
                                          final boolean start,
                                          final String name,
                                          final File directory) throws SailException {
        Sail sail = new AllegroSail(host, port, start, name, directory, 0, 0, false, false);

        sail.initialize();

        return sail;
    }

    private static void doit() throws Exception {
        Sail sail = createAllegroSail("rambo", 4567, false, "btcall.db", new File("/net/rambo/disk8"));
        try {
            String q = DATATYPEPROPERTIES;
            System.out.println(evaluateAndCount(parseQuery(q), sail));
        } finally {
            sail.shutDown();
        }
    }

    public static void main(final String[] args) throws Exception {
        try {
            doit();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }

    ////////////////////////////////////////////////////////////////////////////

    // 12,222 foaf:Image's with dcterms:spatial links
    private static final String GEOTAGGED_IMAGES = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n" +
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n" +
            "SELECT ?s ?o {\n" +
            "?s rdf:type foaf:Image .\n" +
            "?s dcterms:spatial ?o . }";

    private static final String GEOPOINT_TYPES = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
            "PREFIX pos: <http://www.w3.org/2003/01/geo/wgs84_pos#>\n" +
            "SELECT DISTINCT ?z {\n" +
            "?x pos:lat ?y .\n" +
            "?x rdf:type ?z .\n" +
            "}";

    // 24 types of resources are annotated with dcterms:spatial values
    private static final String GEOTAGGED_RESOURCE_TYPES = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
            "PREFIX dcterms: <http://purl.org/dc/terms/>\n" +
            "SELECT DISTINCT ?t {\n" +
            // Note: apparently, AllegroGraph performs best when the most selective clauses appear first in a query.
            "    ?x dcterms:spatial ?y .\n" +
            "    ?x rdf:type ?t .\n" +
            "}";

    private static final String DATATYPEPROPERTIES = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
            "PREFIX owl: <http://www.w3.org/2002/07/owl#>\n" +
            "SELECT DISTINCT ?s {\n" +
            "    ?s rdf:type owl:DatatypeProperty .\n" +
            "}";
}
