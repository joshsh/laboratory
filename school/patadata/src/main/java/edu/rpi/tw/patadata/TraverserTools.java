package edu.rpi.tw.patadata;

import info.aduna.iteration.CloseableIteration;
import org.openrdf.model.Resource;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.impl.LiteralImpl;
import org.openrdf.sail.SailConnection;
import org.openrdf.sail.SailException;

import java.util.Random;

/**
 * User: josh
 * Date: Apr 16, 2010
 * Time: 4:54:51 PM
 */
public class TraverserTools {
    private static final boolean INCLUDE_INFERRED = false;

    private static final int TOTAL_RESOURCES = 408422;

    private static final Random RANDOM = new Random();

    public TraverserTools() {
    }

    public static boolean getStatements(final SailConnection sc,
                                        final Handler<Statement, PataException> handler,
                                        final Resource subject,
                                        final URI predicate,
                                        final Value object) throws PataException {
        try {
            CloseableIteration<? extends Statement, SailException> iter
                    = sc.getStatements(subject, predicate, object, INCLUDE_INFERRED);
            try {
                while (iter.hasNext()) {
                    handler.handle(iter.next());
                }
            } finally {
                iter.close();
            }
        } catch (SailException e) {
            throw new PataException(e);
        }

        return true;
    }

    public static boolean traverseForward(final SailConnection sc,
                                          final Handler<Value, PataException> handler,
                                          final Resource subject,
                                          final URI... predicates) throws PataException {
        final Handler<Statement, PataException> objectGrabber = new Handler<Statement, PataException>() {
            public boolean handle(Statement st) throws PataException {
                return handler.handle(st.getObject());
            }
        };

        for (URI p : predicates) {
            if (!getStatements(sc, objectGrabber, subject, p, null)) {
                return false;
            }

        }

        return true;
    }

    public static boolean traverseBackward(final SailConnection sc,
                                           final Handler<Value, PataException> handler,
                                           final Resource object,
                                           final URI... predicates) throws PataException {
        final Handler<Statement, PataException> subjectGrabber = new Handler<Statement, PataException>() {
            public boolean handle(Statement st) throws PataException {
                return handler.handle(st.getSubject());
            }
        };

        for (URI p : predicates) {
            if (!getStatements(sc, subjectGrabber, null, p, object)) {
                return false;
            }

        }

        return true;
    }

    public static Resource randomResource(final SailConnection sc) throws PataException {
        Resource r;
        int i = RANDOM.nextInt(TOTAL_RESOURCES) + 1;

        try {
            CloseableIteration<? extends Statement, SailException> iter
                    = sc.getStatements(null, Patadata.INDEX, new LiteralImpl("" + i), false);
            try {
                r = iter.next().getSubject();
            } finally {
                iter.close();
            }
        } catch (SailException e) {
            throw new PataException(e);
        }

        return r;
    }
}
