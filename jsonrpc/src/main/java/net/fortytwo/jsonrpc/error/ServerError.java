package net.fortytwo.myotherbrain.web.jsonrpc.error;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:31:18 PM
 */
public class ServerError extends JSONRPCException {
    public ServerError(final int errorCode,
                       final String message) {
        super(errorCode, message);
    }
}