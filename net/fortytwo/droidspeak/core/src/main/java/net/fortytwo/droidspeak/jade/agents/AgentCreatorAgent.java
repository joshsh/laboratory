package net.fortytwo.droidspeak.jade.agents;

import jade.core.Agent;
import jade.core.behaviours.OneShotBehaviour;
import jade.wrapper.AgentController;
import jade.wrapper.ControllerException;
import jade.wrapper.PlatformController;

/**
 * User: josh
 * Date: 2/24/11
 * Time: 5:53 PM
 */
public class AgentCreatorAgent extends Agent {
    public AgentCreatorAgent() {
        addBehaviour(new CreateAgent());
    }

    @Override
    protected void setup() {

    }

    private class CreateAgent extends OneShotBehaviour {
        @Override
        public void action() {
            PlatformController container = getContainerController();
            System.out.println("platform name: " + container.getName());
            try {
                AgentController c = container.createNewAgent("echo2", EchoAgent.class.getName(), new Object[]{});
                c.start();
            } catch (ControllerException e) {
                e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            }
        }
    }
}
