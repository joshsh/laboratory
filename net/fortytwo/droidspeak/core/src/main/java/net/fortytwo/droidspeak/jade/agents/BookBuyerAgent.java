package net.fortytwo.droidspeak.jade.agents;

import jade.core.AID;
import jade.core.Agent;

public class BookBuyerAgent extends Agent {
    private String bookTitle;

    private AID[] sellerAgents = {
            new AID("seller1", AID.ISLOCALNAME),
            new AID("seller2", AID.ISLOCALNAME)};

    protected void setup() {
        System.out.println("Hello! Buyer- agent" + getAID().getName() + " is ready.");

        // Get the title of the book to buy as a start-up argument
        Object[] args = getArguments();
        if (args != null && args.length > 0) {
            bookTitle = (String) args[0];
            System.out.println("Trying to buy " + bookTitle);
        } else {
            // Make the agent terminate immediately
            System.out.println("No book title specified");
            doDelete();
        }
    }

    protected void takeDown() {
        System.out.println("Buyer-agent " + getAID().getName() + " terminating.");
    }
}
