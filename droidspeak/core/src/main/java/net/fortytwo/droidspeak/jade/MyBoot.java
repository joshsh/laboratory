package net.fortytwo.droidspeak.jade;

import jade.core.Profile;
import jade.core.ProfileImpl;
import jade.core.Runtime;
import jade.util.ExtendedProperties;
import jade.util.Logger;
import jade.util.leap.Properties;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class MyBoot {
    public static final String DEFAULT_FILENAME = "leap.properties";
    private static Logger logger = Logger.getMyLogger("jade.Boot");

    public static void main(String args[]) {
        try {
            new MyBoot().doit();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }

    private void doit() throws Exception {
        System.out.println("########" + Runtime.getCopyrightNotice());

        //String agents = "timer:net.fortytwo.droidspeak.jade.TimerAgent;echo:net.fortytwo.droidspeak.jade.EchoAgent";

        Properties config = configure(
                new AgentSpecifier("timer", TimerAgent.class),
                new AgentSpecifier("echo", EchoAgent.class),
                new AgentSpecifier("dictation", DictationAgent.class));
        ProfileImpl profile = new ProfileImpl(config);

        // Start a new JADE runtime system
        Runtime.instance().setCloseVM(true);
        // Check whether this is the Main Container or a peripheral container
        if (profile.getBooleanProperty(Profile.MAIN, true)) {
            Runtime.instance().createMainContainer(profile);
        } else {
            Runtime.instance().createAgentContainer(profile);
        }
    }

    public static Properties configure(final AgentSpecifier... agents) throws IOException {
        Properties props = new ExtendedProperties();

        boolean container = false, backupmain = false, gui = false, nomtp = false;
        String platformName = null, conf = null;
        String mtps = "jade.mtp.xmpp.MessageTransportProtocol";//null;//"1331:java.lang.String";
        Map<String, String> otherProperties = new HashMap<String, String>();

        if (container) {
            props.setProperty(Profile.MAIN, "false");
        }

        if (backupmain) {
            props.setProperty(Profile.LOCAL_SERVICE_MANAGER, "true");
        }

        if (gui) {
            props.setProperty(Profile.GUI, "true");
        }

        if (nomtp) {
            props.setProperty(Profile.NO_MTP, "true");
        }

        if (null != platformName) {
            props.setProperty(Profile.PLATFORM_ID, platformName);
        }

        if (null != mtps) {
            props.setProperty(Profile.MTPS, mtps);
        }

        if (null != conf) {
            props.load(conf);
        }

        for (String key : otherProperties.keySet()) {
            props.setProperty(key, otherProperties.get(key));
        }

        if (0 < agents.length) {
            if (null != props.getProperty(Profile.AGENTS)) {
                System.err.println("WARNING: overriding agents specification set with the \"-agents\" option");
            }

            props.setProperty(Profile.AGENTS, agentPropertyValue(agents));
        }

        // Consistency check
        if ("true".equals(props.getProperty(Profile.NO_MTP))
                && props.getProperty(Profile.MTPS) != null) {
            System.err.println("WARNING: both \"-mtps\" and \"-nomtp\" options specified. The latter will be ignored");
            props.remove(Profile.NO_MTP);
        }

        return props;
    }

    private static String agentPropertyValue(final AgentSpecifier... specifiers) {
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (AgentSpecifier a : specifiers) {
            if (first) {
                first = false;
            } else {
                sb.append(";");
            }
            sb.append(a.nickname).append(":").append(a.clazz.getName());
        }

        System.out.println("agents: " + sb.toString());
        return sb.toString();
    }

    private class AgentSpecifier {
        private final String nickname;
        private final Class clazz;

        public AgentSpecifier(String nickname, Class clazz) {
            this.nickname = nickname;
            this.clazz = clazz;
        }
    }
}

