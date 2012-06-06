package net.fortytwo.laboratory.stardogtesting;

import com.clarkparsia.stardog.api.Connection;
import com.clarkparsia.stardog.api.ConnectionConfiguration;
import com.clarkparsia.stardog.api.Query;
import org.openrdf.model.URI;
import org.openrdf.model.impl.ValueFactoryImpl;
import org.openrdf.query.TupleQueryResult;

import java.util.Random;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class StardogTesting {

    private static final String NODE_URI_PREFIX = "http://franz.com/node-";

    private static final String QUERY = "SELECT DISTINCT ?x ?pa WHERE {" +
            "?x ?pa ?y ." +
            "?y ?pa ?z ." +
            "?z ?pa ?a ." +
            "FILTER(?a != ?y)" +
            "?a ?pa ?x ." +
            "} LIMIT 1";
    /*
    (time
 (dotimes (i 10000)
  (let ((x (aref .nodes. (random .max-nodes.))))
    (select-distinct (?x ?pa)
      (:limit 1)
      (q (?? x) ?pa ?y)
      (q ?y ?pa ?z)
      (q ?z ?pa ?a)
      (notIs ?a ?y)
      (q ?a ?pa (?? x))))))
     */

    public static void main(final String[] args) throws Exception {
        //System.setProperty("stardog.home", "/Users/josh/opt/_stardog/current");
        //System.setProperty("stardog.home", "/disk1/josh/stardog/current");

        //StardogDBMS.startEmbeddedServer();
        try {
            Connection c = ConnectionConfiguration.to("jans-graph-new")
                    .credentials("admin", "admin").url("snarl://localhost:5820").connect();

//            StardogDBMS dbms = StardogDBMS.login("snarl://localhost:5820", "admin", "admin".toCharArray());
            try {
                //dbms.createMemory("memDb");

                Query aQuery = c.query(QUERY);

// now we can run this query...but lets set a limit on it since otherwise that'd be the whole database
                aQuery.limit(10);

                Random rand = new Random();
                rand.setSeed(System.currentTimeMillis());

                int times = 10000;
                for (int i = 0; i < times; i++) {
                    int x = rand.nextInt(10000000);

                    URI aURI = ValueFactoryImpl.getInstance()
                            .createURI(NODE_URI_PREFIX + x);
                    aQuery.parameter("x", aURI);
                    long before = System.currentTimeMillis();
                    TupleQueryResult r = aQuery.executeSelect();
                    int count = 0;
                    try {
                        while (r.hasNext()) {
                            count++;
                            r.next();
                        }
                    } finally {
                        r.close();
                    }
                    long after = System.currentTimeMillis();
                    System.out.println("" + i + ")\t" + count + " results for node " + x + " in " + (after - before) + "ms");
                }
            } finally {
                //    dbms.logout();
                c.close();
            }
        } finally {
            //    StardogDBMS.stopEmbeddedServer();
        }
    }
}
