package net.fortytwo.extendo.demos;

import net.fortytwo.extendo.p2p.ExtendoAgent;
import net.fortytwo.extendo.p2p.SideEffects;
import net.fortytwo.extendo.p2p.osc.OscControl;
import net.fortytwo.extendo.p2p.osc.OscReceiver;
import net.fortytwo.extendo.typeatron.TypeatronControl;

import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class TypeatronControlWrapper {

    protected static final Logger logger = Logger.getLogger(TypeatronControlWrapper.class.getName());

    protected final TypeatronControl typeatron;

    protected TypeatronControlWrapper() throws OscControl.DeviceInitializationException {
        final OscReceiver receiver = new OscReceiver();

        ExtendoAgent agent = null;

        SideEffects environment = new SideEffects() {
            @Override
            public void speak(String message) {
                System.out.println("SPEAK: " + message);
            }

            @Override
            public void setStatus(String message) {
                System.out.println("STATUS: " + message);
            }

            @Override
            public boolean verbose() {
                return false;
            }
        };

        typeatron = new TypeatronControl(receiver, agent, environment);
    }
}
