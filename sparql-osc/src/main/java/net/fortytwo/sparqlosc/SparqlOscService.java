package net.fortytwo.sparqlosc;

import com.illposed.osc.OSCMessage;
import com.illposed.osc.OSCPortOut;
import org.openrdf.model.Value;
import org.openrdf.query.BindingSet;

import java.io.IOException;
import java.util.List;

/**
 * User: josh
 * Date: Jul 31, 2010
 * Time: 4:59:43 PM
 */
public abstract class SparqlOscService {

    public abstract void subscribe(final SparqlOscSubscriber subscriber);

    public abstract void unsubscribe(final SparqlOscSubscriber subscriber);

    protected void handleSparqlResult(final BindingSet result,
                                      final SparqlOscSubscriber subscriber) throws SparqlOscMappingException, IOException {
        OSCMessage msg = bindingSetToOSCMessage(subscriber.getAddressPattern(), result, subscriber.getArguments());

        OSCPortOut sender = new OSCPortOut(subscriber.getInetAddress(), subscriber.getPort());
        sender.send(msg);
    }

    private OSCMessage bindingSetToOSCMessage(final String addressPattern,
                                              final BindingSet set,
                                              final List<SparqlOscArgument> arguments) throws SparqlOscMappingException {
        OSCMessage msg = new OSCMessage(addressPattern);

        for (SparqlOscArgument arg : arguments) {
            Value value = set.getValue(arg.getName());
            Object o = arg.getType().toOscValue(value);
            msg.addArgument(o);
        }

        return msg;
    }

}
