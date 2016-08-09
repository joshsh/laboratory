package com.tinkerpop.blueprints.oupls.sail;

import com.thinkaurelius.titan.core.TitanFactory;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import net.fortytwo.laboratory.GraphFactory;
import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;

public class TitanGraphSailTest extends GraphSailTest {
    protected KeyIndexableGraph createGraph() throws Exception {
        GraphFactory f = new GraphFactory();
        return f.createTitanOnHBase();
        //return f.createTitanOnCassandra("127.0.0.1", "foo3");
    }
}
