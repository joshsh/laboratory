package net.fortytwo.sparqlosc;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortOut;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * An object which translates streaming SPARQL results into OSC messages on the fly.  It connects to an RDF triple store
 * and evaluates SPARQL queries on behalf of downstream OSC applications.  SPARQL results are translated into OSC
 * messages, according to registered mappings, and sent out in real time.
 * 
 * User: josh
 * Date: Jul 31, 2010
 * Time: 4:59:43 PM
 */
public abstract class SparqlOscListener {
    static {
        InputStream resourceAsStream = SparqlOscListener.class.getResourceAsStream("logging.properties");

        try {
            LogManager.getLogManager().readConfiguration(resourceAsStream);
        } catch (SecurityException e) {
            throw new ExceptionInInitializerError(e);
        } catch (IOException e) {
            throw new ExceptionInInitializerError(e);
        }

        LOGGER = Logger.getAnonymousLogger();
    }

    protected static final Logger LOGGER;

    /**
     * Register a new mapping with this listener.
     * @param mapping whenever the listener matches a new result for the query
     * specified in this mapping, it will send out a corresponding OSC message.
     */
    public abstract void register(final SparqlOscMapping mapping);

    /**
     * Unregister a previously registered mapping.
     * @param mapping results for this mapping will no longer be generated, and corresponding OSC messages will no longer be sent out
     */
    public abstract void unregister(final SparqlOscMapping mapping);

    protected void handleSparqlResult(final BindingSet result,
                                      final SparqlOscMapping mapping) throws SparqlOscMappingException, IOException {
        OSCMessage msg = bindingSetToOSCMessage(mapping.getAddressPattern(), result, mapping.getVariables());

        LOGGER.fine("sending OSC message: " + msg);

        OSCPortOut sender = new OSCPortOut(mapping.getInetAddress(), mapping.getPort());
        sender.send(msg);
    }

    private OSCMessage bindingSetToOSCMessage(final String addressPattern,
                                              final BindingSet set,
                                              final List<SparqlOscVariable> variables) throws SparqlOscMappingException {
        OSCMessage msg = new OSCMessage(addressPattern);

        for (SparqlOscVariable arg : variables) {
            Value value = set.getValue(arg.getName());
            Object o = arg.getType().toOscValue(value);
            msg.addArgument(o);
        }

        return msg;
    }

}
