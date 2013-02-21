package net.fortytwo.droidspeak.jade;

import jade.core.Profile;
import jade.core.ProfileImpl;
import jade.core.Runtime;
import jade.mtp.xmpp.MessageTransportProtocol;
import jade.util.ExtendedProperties;
import jade.util.Logger;
import jade.util.leap.Properties;
import jade.wrapper.AgentController;
import net.fortytwo.droidspeak.jade.agents.AgentCreatorAgent;
import net.fortytwo.droidspeak.jade.agents.DictationAgent;
import net.fortytwo.droidspeak.jade.agents.EchoAgent;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class MyBoot {
    public static final String DEFAULT_FILENAME = "leap.properties";
    private static Logger logger = Logger.getMyLogger(MyBoot.class.getName());

    public static void main(String args[]) {
        try {
            Properties props = new Properties();
            props.load(new FileInputStream(new File(args[0])));
            new MyBoot().doit(props);
        } catch (Throwable t) {
            t.printStackTrace(System.err);
        }
    }

    private void doit(final Properties props) throws Exception {
        System.out.println("########" + Runtime.getCopyrightNotice());

        //String agents = "timer:net.fortytwo.droidspeak.jade.TimerAgent;echo:net.fortytwo.droidspeak.jade.agents.EchoAgent";

        Properties config = configure(
                new AgentSpecifier("timer", TimerAgent.class),
                new AgentSpecifier("echo", EchoAgent.class),
                new AgentSpecifier("dictation", DictationAgent.class),
                new AgentSpecifier("creator", AgentCreatorAgent.class));
        for (String key : props.stringPropertyNames()) {
            config.setProperty(key, props.getProperty(key));
        }
        ProfileImpl profile = new ProfileImpl(config);

        // Start a new JADE runtime system
        Runtime.instance().setCloseVM(true);
        // Check whether this is the Main Container or a peripheral container
        jade.wrapper.AgentContainer container;
        if (profile.getBooleanProperty(Profile.MAIN, true)) {
            container = Runtime.instance().createMainContainer(profile);
        } else {
            container = Runtime.instance().createAgentContainer(profile);
        }

        //System.out.println("you should never see this");

        AgentController c = container.createNewAgent("echo69", EchoAgent.class.getName(), new Object[]{});
        c.start();

        new SimpleExample().doSomeStuff(container);
    }

    public static Properties configure(final AgentSpecifier... agents) throws IOException {
        Properties props = new ExtendedProperties();

        boolean container = false, backupmain = false, gui = false, nomtp = false;
        String platformName = null, conf = null;

        //String mtps = XmppMessageTransportProtocol.class.getName();
        String mtps = MessageTransportProtocol.class.getName();

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

