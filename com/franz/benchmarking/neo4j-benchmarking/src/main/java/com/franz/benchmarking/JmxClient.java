package com.franz.benchmarking;

import javax.management.MBeanServerConnection;
import javax.management.ObjectInstance;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class JmxClient {
    public static void main(final String[] args) {
        try {
            logJmxStats(1000);
        } catch (Exception e) {
            e.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private static void logJmxStats(final long interval) throws Exception {
        Map<String, String[]> env = new HashMap<String, String[]>();
        env.put(JMXConnector.CREDENTIALS, new String[]{"user", "pass"});

        JMXServiceURL address = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://65.50.203.145:1331/jmxrmi");
        //JMXServiceURL address = new JMXServiceURL("service:rmi:///jndi/rmi://localhost:1331/jmxrmi");
        //JMXServiceURL address = new JMXServiceURL("service:jmx:rmi:///jndi/rmi://localhost:1331/myname");
        //JMXServiceURL address = new JMXServiceURL("service:jmx:rmi://localhost:1331");

        JMXConnector connector = JMXConnectorFactory.connect(address, env);
        MBeanServerConnection mbs = connector.getMBeanServerConnection();

        while (true) {
            //MBeanServer connection = ManagementFactory.getPlatformMBeanServer();
            Set<ObjectInstance> set = mbs.queryMBeans(new ObjectName("java.lang:type=Memory"), null);
            ObjectInstance oi = set.iterator().next();

            StringBuilder sb = new StringBuilder();
            Object attrValue;

            sb.append(System.currentTimeMillis());
            sb.append("\t");

            attrValue = getAttributeValue(mbs, oi, "HeapMemoryUsage");
            sb.append(((CompositeData) attrValue).get("used").toString());
            sb.append("\t");
            sb.append(((CompositeData) attrValue).get("max").toString());
            attrValue = getAttributeValue(mbs, oi, "NonHeapMemoryUsage");
            sb.append("\t");
            sb.append(((CompositeData) attrValue).get("used").toString());
            sb.append("\t");
            sb.append(((CompositeData) attrValue).get("max").toString());

            //System.out.println(((CompositeData) attrValue).get("used").toString());

            System.out.println(sb.toString());

            Thread.sleep(interval);
        }

        /*
        if (true) return;

//get all mbeans
        Set<ObjectInstance> beans = mbs.queryMBeans(null, null);

        for (ObjectInstance instance : beans) {
            MBeanInfo info = mbs.getMBeanInfo(instance.getObjectName());

            System.out.println("info:");
            //System.out.println("info: " + info);
            for (MBeanAttributeInfo i : info.getAttributes()) {
                System.out.println("\t" + i.getName() + " (" + i.getType() + "): " + i.getDescription());

            }
        }
        */
    }

    private static Object getAttributeValue(final MBeanServerConnection mbs, final ObjectInstance oi, final String name) throws Exception {
        Object attrValue = mbs.getAttribute(oi.getObjectName(), name);
        if (!(attrValue instanceof CompositeData)) {
            throw new Exception("attribute value is instanceof [" + attrValue.getClass().getName() +
                    ", exiting -- must be CompositeData.");
        }

        return attrValue;
    }
}
