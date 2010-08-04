package net.fortytwo.sparqlosc;

import org.openrdf.model.Literal;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.model.BNode;
import org.openrdf.model.vocabulary.XMLSchema;

/**
 * A data type for SPARQL-to-OSC mappings which constrains the translation of RDF values to OSC values.
 *
 * User: josh
 * Date: Aug 3, 2010
 * Time: 2:43:33 PM
 */
public enum OscType {
    INT32,  // OSC's Int32 types, which is compatible with xsd:integer and xsd:int
    FLOAT32,  // OSC's Float32 type, whcih is compatible with xsd:float and xsd:double
    STRING;  // OSC's String type, which is used here as a catch-all for many non-numeric types
    // TODO: blob, timetag

    private static final String
            NULL_ARGUMENT = "[null]";

    public Object toOscValue(final Value v) throws SparqlOscMappingException {
        if (null == v) {
            return NULL_ARGUMENT;
        }

        if (v instanceof BNode) {
            throw new SparqlOscMappingException("blank node in result");
        }

        switch (this) {
            case FLOAT32:
                return convertFloatValue(v);
            case INT32:
                return convertIntValue(v);
            case STRING:
                return convertStringValue(v);
            default:
                throw new IllegalStateException("unexpected OSC type: " + this);
        }
    }

    private static Object convertFloatValue(final Value v) throws SparqlOscMappingException {
        if (!(v instanceof Literal)) {
            throw new SparqlOscMappingException("expected a literal value");
        }
        URI datatype = ((Literal) v).getDatatype();
        if (null == datatype) {
            throw new SparqlOscMappingException("expected a typed literal value");
        }
        String label = ((Literal) v).getLabel();
        if (datatype.equals(XMLSchema.FLOAT)) {
            try {
                return Float.valueOf(label);
            } catch (Throwable t) {
                throw new SparqlOscMappingException("could not convert float-typed value to float", t);
            }
        } else if (datatype.equals(XMLSchema.DOUBLE)) {
            // TODO: will this work?
            try {
                return Float.valueOf(label);
            } catch (Throwable t) {
                throw new SparqlOscMappingException("could not convert double-typed value to float", t);
            }
        } else {
            throw new SparqlOscMappingException("data type could not be converted to float: " + datatype);
        }
    }

    private static Object convertIntValue(final Value v) throws SparqlOscMappingException {
        if (!(v instanceof Literal)) {
            throw new SparqlOscMappingException("expected a literal value");
        }
        URI datatype = ((Literal) v).getDatatype();
        if (null == datatype) {
            throw new SparqlOscMappingException("expected a typed literal value");
        }
        String label = ((Literal) v).getLabel();
        if (datatype.equals(XMLSchema.INT) || datatype.equals(XMLSchema.INTEGER)) {
            try {
                return Integer.valueOf(label);
            } catch (Throwable t) {
                throw new SparqlOscMappingException("could not convert integer-typed value to integer", t);
            }
        } else {
            throw new SparqlOscMappingException("data type could not be converted to integer: " + datatype);
        }
    }

    // Note: for now, the STRING type matches almost everything
    private static Object convertStringValue(final Value v) throws SparqlOscMappingException {
        if (v instanceof Literal) {
            String label = ((Literal) v).getLabel();
            if (null == label) {
                throw new SparqlOscMappingException("null literal label");
            }

            return label;
        } else if (v instanceof URI) {
            return v.toString();
        } else {
            throw new SparqlOscMappingException("value has unfamiliar type: " + v.getClass());
        }
    }
}
