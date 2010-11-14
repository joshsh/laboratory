package net.fortytwo.myotherbrain.web.jsonrpc.error;

/**
 * User: josh
 * Date: Nov 14, 2010
 * Time: 1:50:57 PM
 */
public class InvalidResponseException extends Exception {
    public InvalidResponseException(final Throwable cause) {
        super(cause);
    }

    public InvalidResponseException(final String message) {
        super(message);
    }
}
