package net.fortytwo.droidspeak.jade.agents;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.lang.acl.ACLMessage;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 9:39:26 PM
 */
public class BarAgent extends Agent {
    private final AID myBuddy = new AID("foo", AID.ISLOCALNAME);

    public BarAgent() {
        System.out.println("# bar");
        addBehaviour(new SendAMessage());
    }

    private class SendAMessage extends OneShotBehaviour {
        public void action() {
            ACLMessage m = new ACLMessage(ACLMessage.INFORM);
            m.addReceiver(myBuddy);
            m.setLanguage("English");
            //m.setOntology("Weather-forecast-ontology");
            m.setContent("Today it’s raining");
            send(m);
        }
    }
}
