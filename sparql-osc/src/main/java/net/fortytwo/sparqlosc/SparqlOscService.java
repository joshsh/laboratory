package net.fortytwo.sparqlosc;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortOut;
import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.vocabulary.XMLSchema;
import org.openrdf.query.BindingSet;

import java.io.IOException;

/**
 * User: josh
 * Date: Jul 31, 2010
 * Time: 4:59:43 PM
 */
public abstract class SparqlOscService {
    private static final String
            NULL_ARGUMENT = "[null]";

    public abstract void subscribe(final SparqlOscSubscriber subscriber);

    protected void handleSparqlResult(final BindingSet result,
                                      final SparqlOscSubscriber subscriber) throws SparqlOscMappingException, IOException {
        OSCMessage msg = bindingSetToOSCMessage(subscriber.getAddressPattern(), result, subscriber.getVariableNames());

        OSCPortOut sender = new OSCPortOut(subscriber.getInetAddress(), subscriber.getPort());
        sender.send(msg);
    }

    private OSCMessage bindingSetToOSCMessage(final String addressPattern,
                                              final BindingSet set,
                                              final String[] variableNames) throws SparqlOscMappingException {
        OSCMessage msg = new OSCMessage(addressPattern);

        for (String key : variableNames) {
            Value value = set.getValue(key);
            Object arg = rdfValueToOscArgument(value);
            msg.addArgument(arg);
        }

        return msg;
    }

    private Object rdfValueToOscArgument(final Value v) throws SparqlOscMappingException {
        if (null == v) {
            return NULL_ARGUMENT;
        } else if (v instanceof URI) {
            String s = ((URI) v).toString();
            if (s.length() < 1) {
                throw new SparqlOscMappingException("empty URI");
            } else {
                return s;
            }
        } else if (v instanceof Literal) {
            // TODO: BigInteger?
            String label = ((Literal) v).getLabel();
            if (null == label) {
                throw new SparqlOscMappingException("null literal label");
            }
            URI datatype = ((Literal) v).getDatatype();
            if (null == datatype) {
                if (label.length() < 1) {
                    throw new SparqlOscMappingException("empty literal label");
                }
                return label;
            } else if (datatype.equals(XMLSchema.INT) || datatype.equals(XMLSchema.INTEGER)) {
                try {
                    return Integer.valueOf(label);
                } catch (Throwable t) {
                    throw new SparqlOscMappingException("could not convert integer-typed value to integer", t);
                }
            } else if (datatype.equals(XMLSchema.FLOAT)) {
                try {
                    return Float.valueOf(label);
                } catch (Throwable t) {
                    throw new SparqlOscMappingException("could not convert float-typed value to float", t);
                }
            } else if (datatype.equals(XMLSchema.DOUBLE)) {
                // TODO: will this work?
                try {
                    return Float.valueOf(label);
                } catch (Throwable t) {
                    throw new SparqlOscMappingException("could not convert double-typed value to float", t);
                }
            } else if (datatype.equals(XMLSchema.BOOLEAN)) {
                return label.equals("true");
            } else {
                return label;
            }
        } else if (v instanceof BNode) {
            throw new SparqlOscMappingException("blank node in result");
        } else {
            throw new SparqlOscMappingException("value has unfamiliar type: " + v.getClass());
        }
    }

}
