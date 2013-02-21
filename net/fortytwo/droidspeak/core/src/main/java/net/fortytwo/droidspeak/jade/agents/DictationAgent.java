package net.fortytwo.droidspeak.jade.agents;

import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.domain.FIPAException;
import jade.lang.acl.ACLMessage;

import java.util.Iterator;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 9:57:36 PM
 */
public class DictationAgent extends Agent {
    public DictationAgent() {
        System.out.println("# dictation");
        this.addBehaviour(new Dictation());
    }

    @Override
    protected void setup() {
        DFAgentDescription dfd = new DFAgentDescription();
        dfd.setName(getAID());
        ServiceDescription sd = new ServiceDescription();
        sd.setType("message-dictation");
        sd.setName("message dictation...");
        dfd.addServices(sd);
        try {
            DFService.register(this, dfd);
            //System.out.println("registered successfully");
        } catch (FIPAException e) {
            e.printStackTrace();
        }

        System.out.println("dictation service (" + this.getAID().getName() + ") is available here:");
        Iterator i =  this.getAID().getAllAddresses();
        while (i.hasNext()) {
            System.out.println("\t" + i.next());
        }
    }

    protected void takeDown() {
        try {
            DFService.deregister(this);
        }
        catch (FIPAException e) {
            e.printStackTrace();
        }
    }

    private class Dictation extends CyclicBehaviour {
        public void action() {
            ACLMessage m = myAgent.receive();
            if (null != m) {
                System.out.println("# received message: " + m);
            } else {
                block();
            }
        }
    }
}