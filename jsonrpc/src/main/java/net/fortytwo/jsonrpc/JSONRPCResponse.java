package net.fortytwo.myotherbrain.web.jsonrpc;

import org.json.JSONObject;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 6:59:28 PM
 */
public class JSONRPCResponse {
    private final Object id;
    private final String method;
    private final Object result;
    private final JSONRPCError error;

    public JSONRPCResponse(final JSONRPCRequest request,
                           final Object result) {
        id = request.getId();    
    }

    public JSONObject toJSON() {
        
    }
}
