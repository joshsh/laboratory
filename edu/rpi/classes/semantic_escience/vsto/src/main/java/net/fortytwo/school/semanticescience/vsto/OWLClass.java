package net.fortytwo.school.semanticescience.vsto;

import org.openrdf.model.URI;

import java.util.Collection;
import java.util.LinkedList;

/**
 * Created by IntelliJ IDEA.
* User: josh
* Date: Sep 13, 2008
* Time: 7:42:17 PM
* To change this template use File | Settings | File Templates.
*/
class OWLClass {
    public Collection<OWLClass> superClasses = new LinkedList<OWLClass>();
    public URI uri;

    public boolean inherits(final OWLClass ancestor) {
        for (OWLClass parent : superClasses) {
            if (parent.uri.equals(ancestor.uri)) {
                return true;
            }

            if (parent.inherits(ancestor)) {
                return true;
            }
        }

        return false;
    }
}
