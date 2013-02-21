package net.fortytwo.droidspeak.jade.agents;

import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;

public class FooAgent extends Agent {
    public FooAgent() {
        System.out.println("# foo");
        this.addBehaviour(new ReceiveMessages());
    }

    private class ReceiveMessages extends CyclicBehaviour {
        public void action() {
            ACLMessage m = myAgent.receive();
            if (null != m) {
                System.out.println("got a message: " + m);    
            }
        }
    }
}