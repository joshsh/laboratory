package edu.rpi.twc.sesamestream;

import info.aduna.io.IOUtil;
import edu.rpi.twc.sesamestream.util.StatementListBuilder;
import org.openrdf.model.Statement;
import org.openrdf.query.BindingSet;
import org.openrdf.query.parser.ParsedQuery;
import org.openrdf.query.parser.QueryParser;
import org.openrdf.query.parser.sparql.SPARQLParser;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.ntriples.NTriplesParser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TestRunner {
    public static void main(final String[] args) throws Exception {
        if (2 != args.length) {
            printUsageAndExit();
        }

        List<String> queryFiles = getLines(args[0]);
        List<String> dataFiles = getLines(args[1]);

        QueryEngine engine = new QueryEngine();
        QueryParser queryParser = new SPARQLParser();
        String baseUri = "http://example.org/base-uri/";

        BindingSetHandler bsh = new BindingSetHandler() {
            public void handle(BindingSet result) {
                // Do nothing.
            }
        };

        for (String f : queryFiles) {
            System.out.println("adding query file " + f);
            InputStream in = new FileInputStream(new File(f));
            try {
                String query = IOUtil.readString(in);
                ParsedQuery pq = queryParser.parseQuery(query, baseUri);

                engine.addQuery(pq.getTupleExpr(), bsh);
            } finally {
                in.close();
            }
        }

        for (String f : dataFiles) {
            System.out.println("adding data file " + f);

            StatementListBuilder h = new StatementListBuilder();

            InputStream in = new FileInputStream(new File(f));
            try {
                RDFParser p = new NTriplesParser();
                p.setRDFHandler(h);
                p.parse(in, baseUri);
            } finally {
                in.close();
            }

            for (Statement s : h.getStatements()) {
                engine.addStatement(s);
            }
        }
    }

    private static List<String> getLines(final String fileName) throws IOException {
        List<String> lines = new LinkedList<String>();
        InputStream in = new FileInputStream(new File(fileName));
        try {
            BufferedReader b = new BufferedReader(new InputStreamReader(in));
            String line;
            while (null != (line = b.readLine())) {
                lines.add(line.trim());
            }
        } finally {
            in.close();
        }

        return lines;
    }

    private static void printUsageAndExit() {
        System.err.println("Usage: TestRunner list-of-queries.txt list-of-data-files.txt");
        System.exit(1);
    }
}
