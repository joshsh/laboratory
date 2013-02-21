package net.fortytwo.droidspeak.jade;

import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.OneShotBehaviour;
import jade.wrapper.AgentContainer;
import jade.wrapper.AgentController;
import net.fortytwo.droidspeak.jade.agents.EchoAgent;

/**
 * User: josh
 * Date: 2/24/11
 * Time: 6:13 PM
 */
public class SimpleExample {
    private String foo = "forty-two.";

    public void doSomeStuff(AgentContainer container) throws Exception {
        Thread.sleep(1000);

        System.out.println("creating a new agent");
        AgentController c = container.createNewAgent("myagent42", EchoAgent.class.getName(), new Object[]{});
        c.start();
    }

    public class MyAgent extends Agent {
        public MyAgent() {
            addBehaviour(new MyBehavior());
        }
    }

    private class MyBehavior extends OneShotBehaviour {
        @Override
        public void action() {
            System.out.println("foo = " + foo);
        }
    }
}
