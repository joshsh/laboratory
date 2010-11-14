package net.fortytwo.myotherbrain.web.jsonrpc.error;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:12:04 PM
 */
public class JSONRPCException extends Exception {
    private final int errorCode;

    public JSONRPCException(final int errorCode,
                            final String message) {
        super(message);
        this.errorCode = errorCode;        
    }

    public int getErrorCode() {
        return errorCode;
    }
}
