package net.fortytwo.droidspeak.jade;

import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.domain.FIPAException;
import jade.lang.acl.ACLMessage;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 9:57:36 PM
 */
public class EchoAgent extends Agent {
    public EchoAgent() {
        System.out.println("# echo");
        this.addBehaviour(new Echo());
    }

    protected void setup() {
        DFAgentDescription dfd = new DFAgentDescription();
        dfd.setName(getAID());
        ServiceDescription sd = new ServiceDescription();
        sd.setType("message-echoing");
        sd.setName("message echoing...");
        dfd.addServices(sd);
        try {
            DFService.register(this, dfd);
            //System.out.println("registered successfully");
        } catch (FIPAException fe) {
            fe.printStackTrace();
        }
    }

    protected void takeDown() {
        // Deregister from the yellow pages
        try {
            DFService.deregister(this);
        }
        catch (FIPAException fe) {
            fe.printStackTrace();
        }
        // Close the GUI
        //myGui.dispose();
        // Printout a dismissal message
        System.out.println("Seller-agent " + getAID().getName() + " terminating.");
    }

    private class Echo extends CyclicBehaviour {
        public void action() {
            ACLMessage m = myAgent.receive();
            if (null != m) {
                //System.out.println("# echoing: " + m);
                ACLMessage r = m.createReply();
                r.setLanguage(m.getLanguage());
                r.setContent(m.getContent());
                send(r);
            } else {
                block();
            }
        }
    }
}
