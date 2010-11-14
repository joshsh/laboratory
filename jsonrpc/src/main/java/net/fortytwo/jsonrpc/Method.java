package net.fortytwo.jsonrpc;

import net.fortytwo.jsonrpc.error.InvalidRequestError;
import net.fortytwo.jsonrpc.error.ServerError;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.HashMap;
import java.util.Map;

/**
 * A method which can be invoked remotely.
 * <p/>
 * User: josh
 * Date: Nov 4, 2010
 * Time: 9:02:37 PM
 */
public abstract class Method {
    /**
     * @return a String containing the name of the method to be invoked. Method
     *         names that begin with the word rpc followed by a period character
     *         (U+002E or ASCII 46) are reserved for rpc-internal methods and
     *         extensions and MUST NOT be used for anything else.
     */
    public abstract String getName();

    /**
     * @param params the arguments to the method (optional)
     * @return the result of the execution. It must be a primitive or structured
     *         JSON object. For notifications, the result may be null.
     * @throws net.fortytwo.jsonrpc.error.InvalidParamsError
     *                     if the given parameters cannot be accepted
     * @throws ServerError if an application-specific error occurs
     */
    public Object execute(final JSONObject params) throws JSONRPCError {
        Map<String, Object> p = new HashMap<String, Object>();
        JSONArray names = params.names();
        try {
            for (int i = 0; i < names.length(); i++) {
                String name = names.getString(i);
                Object o = params.get(name);
                if (!JSONRPC.isValidObject(o)) {
                    throw new InvalidRequestError("value of parameter '" + name + "' is invalid");
                }
                p.put(name, o);
            }
        } catch (JSONException e) {
            throw new InvalidRequestError(e.getMessage());
        }

        return execute(p);
    }

    /**
     * @param params the arguments to the method (optional)
     * @return the result of the execution. It must be a primitive or structured
     *         JSON object. For notifications, the result may be null.
     * @throws net.fortytwo.jsonrpc.error.InvalidParamsError
     *                     if the given parameters cannot be accepted
     * @throws ServerError if an application-specific error occurs
     */
    public Object execute(final JSONArray params) throws JSONRPCError {
        Object p[] = new Object[params.length()];
        try {
            for (int i = 0; i < p.length; i++) {
                Object o = params.get(i);
                p[i] = o;
            }
        } catch (JSONException e) {
            throw new InvalidRequestError(e.getMessage());
        }

        return execute(p);
    }

    /**
     * Override this method.
     *
     * @param orderedParams an ordered list of parameters
     * @return the result of invocation
     * @throws ServerError if an application-specific error is generated, or if
     *                     this method is called directly (without having been
     *                     overridden in the inheriting class)
     */
    protected Object execute(Object... orderedParams) throws ServerError {
        throw new ServerError(JSONRPCError.Type.ORDERED_PARAMETERS_NOT_SUPPORTED.getCode(),
                "ordered parameters not supported by this method", null);
    }

    /**
     * Override this method.
     *
     * @param namedParams a set of key/value parameters
     * @return the result of invocation
     * @throws ServerError if an application-specific error is generated, or if
     *                     this method is called directly (without having been
     *                     overridden in the inheriting class)
     */
    protected Object execute(Map<String, Object> namedParams) throws ServerError {
        throw new ServerError(JSONRPCError.Type.NAMED_PARAMETERS_NOT_SUPPORTED.getCode(),
                "ordered parameters not supported by this method", null);
    }
}
