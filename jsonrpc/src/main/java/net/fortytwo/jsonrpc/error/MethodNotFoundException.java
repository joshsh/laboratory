package net.fortytwo.myotherbrain.web.jsonrpc.error;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class MethodNotFoundException extends JSONRPCException {
    private final String name;

    public MethodNotFoundException(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}