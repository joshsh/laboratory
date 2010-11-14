package net.fortytwo.myotherbrain.web.jsonrpc;

/**
 * User: josh
 * Date: Nov 4, 2010
 * Time: 6:53:50 PM
 */
public class JSONRPC {
    /**
     * JSON-RPC error codes and ranges
     */
    public static final int
            PARSE_ERROR = -32700,
            INVALID_REQUEST = -32600,
            METHOD_NOT_FOUND = -32601,
            INVALID_PARAMS = -32602,
            INTERNAL_ERROR = -32603,
            SERVER_ERROR_MIN = -32099,
            SERVER_ERROR_MAX = -32000;

    /**
     * Implementation-specific error codes
     */
    public static final int
            NON_JSON_RESULT = -32001;

    public static final String VERSION = "2.0";
}
