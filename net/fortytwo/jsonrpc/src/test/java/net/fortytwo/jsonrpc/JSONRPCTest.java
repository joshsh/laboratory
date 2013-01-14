package net.fortytwo.jsonrpc;

import junit.framework.TestCase;

/**
 * User: josh
 * Date: Nov 14, 2010
 * Time: 8:33:58 PM
 */
public class JSONRPCTest extends TestCase {
    private static final JSONRPCServer SERVER = new MockJSONRPCServer();

    private static String REQUEST_TEMPLATE = "{\"jsonrpc\":\"2.0\", \"id\":42, \"method\":METHOD, \"params\":PARAMS}";

    private static String NAMED_PARAMS_REQUEST = "{\"jsonrpc\":\"2.0\", \"id\":42," +
            " \"method\":\"exampleMethodWithNamedParams\"," +
            " \"params\":{\"first\":\"ABC\", \"second\":\"123\"}}";
    private static String ORDERED_PARAMS_REQUEST = "{\"jsonrpc\":\"2.0\", \"id\":42," +
            " \"method\":\"exampleMethodWithOrderedParams\"," +
            " \"params\":[\"ABC\", \"123\"]}";

    public void testTemp() throws Exception {
        execute(NAMED_PARAMS_REQUEST);
        execute(ORDERED_PARAMS_REQUEST);    
    }

    private void execute(final String request) {
        String response = SERVER.handle(request);
        System.out.println(response);
    }
}
