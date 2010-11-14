package net.fortytwo.myotherbrain.web.jsonrpc.error;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 8:13:48 PM
 */
public class InvalidRequestException extends JSONRPCException {
     public InvalidRequestException(final String message) {
         super(message);
     }
}
