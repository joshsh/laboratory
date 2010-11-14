package net.fortytwo.myotherbrain.web.jsonrpc;

import org.json.JSONObject;
import net.fortytwo.myotherbrain.web.jsonrpc.error.InvalidParamsException;
import net.fortytwo.myotherbrain.web.jsonrpc.error.ServerError;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 9:02:37 PM
 */
public interface Method {
    String getName();

    Object execute(JSONObject params) throws InvalidParamsException, ServerError;
}
