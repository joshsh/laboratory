package edu.rpi.tw.patadata;

import org.openrdf.model.Value;
import org.openrdf.model.Resource;

/**
 * User: josh
* Date: Apr 19, 2010
* Time: 3:22:49 PM
*/
public class KeepResourcesFilter extends Pipe<Value, Resource, PataException> {
    public KeepResourcesFilter(final Handler<Resource, PataException> innerHandler) {
        super(innerHandler);
    }

    public boolean handle(final Value value) throws PataException {
        return !(value instanceof Resource) || innerHandler.handle((Resource) value);
    }
}
