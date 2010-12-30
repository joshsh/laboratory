package net.fortytwo.droidspeak.jade;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.domain.FIPAException;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import java.util.Iterator;

/**
 * User: josh
 * Date: Dec 28, 2010
 * Time: 10:02:49 PM
 */
public class TimerAgent extends Agent {
    //private final AID echo = new AID("echo", AID.ISLOCALNAME);
    private final AID echo;

    private final MessageTemplate template;
    private final String convo = "timerConvo";

    public TimerAgent() {
        System.out.println("# timer");
        this.addBehaviour(new TimeRoundTrip(1));
        template = MessageTemplate.MatchConversationId(convo);

        echo = new AID();
        echo.setName("echo@droidspeak1");
        //echo.addAddresses("http://fortytwo.net:7778/acc");
        echo.addAddresses("xmpp://patabot.1@fortytwo.net/acc");
    }

    protected void setup() {
        System.out.println("timer service is available here:");
        Iterator i = this.getAID().getAllAddresses();
        while (i.hasNext()) {
            System.out.println("\t" + i.next());
        }
    }

    private class TimeRoundTrip extends Behaviour {
        private int count = -1;
        private final int total;
        private boolean isDone = false;
        private final ACLMessage message;
        private long startTime;

        public TimeRoundTrip(final int total) {
            this.total = total;

            message = new ACLMessage(ACLMessage.INFORM);
            message.setSender(getAID());
            //message.addReceiver(echo);
            message.setLanguage("English");
            message.setContent("timer test");
            message.setConversationId(convo);
        }

        public void action() {
            if (-1 == count) {
                // TODO: temporary
                sendDictationMessage();

                if (null == echo) {
                    DFAgentDescription template = new DFAgentDescription();
                    ServiceDescription sd = new ServiceDescription();
                    sd.setType("message-echoing");
                    template.addServices(sd);
                    try {
                        DFAgentDescription[] result = DFService.search(myAgent, template);
                        if (0 == result.length) {
                            System.err.println("no candidate recipients found!");
                        } else {
                            AID recipient = result[0].getName();
                            message.addReceiver(recipient);
                        }
                    } catch (FIPAException fe) {
                        fe.printStackTrace();
                    }
                } else {
                    message.addReceiver(echo);
                }

                count++;
                startTime = System.currentTimeMillis();
                //System.out.println("# sending first");
                send(message);
            } else {
                ACLMessage r = receive(template);
                //System.out.println("# r = " + r);

                if (null != r) {
                    count++;
                    System.out.println("# received " + count);
                    if (total == count) {
                        long duration = System.currentTimeMillis() - startTime;
                        System.out.println("" + total + " message round-trips in " + duration + "ms ("
                                + (duration / (1.0 * total)) + " ms / message)");
                        isDone = true;
                    } else {
                        //System.out.println("# sending next");
                        send(message);
                    }
                } else {
                    block();
                }
            }
        }

        public boolean done() {
            return isDone;
        }
    }

    private void sendDictationMessage() {
        try {
            AID dictation = new AID();
            dictation.setName("dictation@droidspeak1");
//            dictation.addAddresses("http://127.0.0.1:54542/acc");
            //dictation.addAddresses("http://fortytwo.net:7778/acc");
            dictation.addAddresses("xmpp://patabot.1@fortytwo.net/acc");
            ACLMessage m = new ACLMessage(ACLMessage.INFORM);
            m.setSender(getAID());
            m.setLanguage("English");
            m.setContent("dictation test");
            m.addReceiver(dictation);

            send(m);

            System.out.println("dictation test message queued");
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }
}
