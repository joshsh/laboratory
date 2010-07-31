package net.fortytwo.sparqlosc;

import org.openrdf.sail.Sail;
import org.openrdf.sail.memory.MemoryStore;

/**
 * User: josh
 * Date: Jul 31, 2010
 * Time: 4:42:33 PM
 */
public class SPARQLOSC {
    public static void main(final String[] args) throws Exception {

        try {
            SPARQLOSC x = new SPARQLOSC();
            x.tryItOut();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }

    private void tryItOut() throws Exception {
        Sail sail = new MemoryStore();
    }
}
