package net.fortytwo.sesamize;

import gnu.getopt.Getopt;
import gnu.getopt.LongOpt;
import org.openrdf.repository.Repository;
import org.openrdf.repository.RepositoryConnection;
import org.openrdf.repository.RepositoryException;
import org.openrdf.repository.sail.SailRepository;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandler;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.Rio;
import org.openrdf.sail.Sail;
import org.openrdf.sail.SailException;
import org.openrdf.sail.nativerdf.NativeStore;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * User: josh
 * Date: Jul 2, 2010
 * Time: 8:23:16 PM
 */
public class Sesamize {
    private static final String
            NAME = "Sesamize",
            VERSION = "0.1";

    private static boolean quiet;

    private enum Command {
        DUMP("dump"),
        IMPORT("import");

        private final String name;

        private Command(final String name) {
            this.name = name;
        }

        public static Command lookup(final String name) {
            for (Command c : Command.values()) {
                if (c.name.equals(name)) {
                    return c;
                }
            }

            return null;
        }
    }

    private static final Map<String, RDFFormat> rdfFormatByName;

    static {
        rdfFormatByName = new HashMap<String, RDFFormat>();
        rdfFormatByName.put("rdfxml", RDFFormat.RDFXML);
        rdfFormatByName.put("rdf/xml", RDFFormat.RDFXML);
        rdfFormatByName.put("rdf", RDFFormat.RDFXML);
        rdfFormatByName.put("xml", RDFFormat.RDFXML);
        rdfFormatByName.put("trig", RDFFormat.TRIG);
        rdfFormatByName.put("turtle", RDFFormat.TURTLE);
        rdfFormatByName.put("trix", RDFFormat.TRIX);
        rdfFormatByName.put("ntriples", RDFFormat.NTRIPLES);
        rdfFormatByName.put("ntriple", RDFFormat.NTRIPLES);
    }

    private static RDFFormat findRDFFormat(final String name) {
        return rdfFormatByName.get(name);
    }

    private static void printUsage() {
        System.out.println("Usage:  sesamize [options] command [arguments]");
        System.out.println("Options:\n"
                + "  -h           Print this help and exit\n"
                + "  -q           Suppress normal output\n"
                + "  -v           Print version information and exit");
        System.out.println("For more information, please see:\n"
                + "  <URL:http://github.com/joshsh/laboratory/tree/master/sesamize>.");
    }

    private static void badUsage() {
        printUsage();
        System.exit(1);
    }

    private static void printVersion() {
        System.out.println(NAME + " " + VERSION);
    }

    public static void main(final String[] args) {
        Args a = new Args(Arrays.copyOfRange(args, 1, args.length));
        //Args a = new Args(args);
        System.out.println("command = " + args[0]);
        Command c = Command.lookup(args[0]);

        if (null == c) {
            badUsage();
        }

        try {
            switch (c) {
                case DUMP:
                    doDump(a);
                    break;
                case IMPORT:
                    doImport(a);
                    break;
            }
        } catch (Throwable t) {
            System.out.println("Exited with error: " + t);
            t.printStackTrace();
            System.exit(1);
        }
        //String[] newArgs = new String[]{"sesamize", "-v", "merge"};
        //mainOld(newArgs);
    }

    public static void mainOld(final String[] args) {
        // Default values.
        quiet = false;
        boolean showVersion = false, showHelp = false;
        //File inputFile = null;
        Command command;

        // Long options are available but are not advertised.
        LongOpt[] longOptions = {
                new LongOpt("help", LongOpt.NO_ARGUMENT, null, 'h'),
                new LongOpt("quiet", LongOpt.NO_ARGUMENT, null, 'q'),
                new LongOpt("version", LongOpt.NO_ARGUMENT, null, 'v')};

        Getopt g = new Getopt(NAME, args, "hqv", longOptions);
        int c;
        while ((c = g.getopt()) != -1) {
            switch (c) {
                case 'h':
                case 0:
                    showHelp = true;
                    break;
                case 'q':
                case 1:
                    quiet = true;
                    break;
                case 'v':
                case 2:
                    showVersion = true;
                    break;
                case '?':
                    // Note: getopt() already printed an error
                    printUsage();
                    System.exit(1);
                    break;
                default:
                    System.err.print("getopt() returned " + c + "\n");
            }
        }

        int i = g.getOptind();
        if (i < args.length) {
            // Too many non-option arguments.
            if (args.length - i > 2) {
                printUsage();
                System.exit(1);
            }

            //inputFile = new File(args[i]);
            System.out.println("a -> " + args[i]);
            command = Command.lookup(args[i].toLowerCase());
            if (null == command) {
                System.out.println("found command: " + command);
                printUsage();
                System.exit(1);
            }
        }

        if (showHelp) {
            printUsage();
            System.exit(0);
        }

        if (showVersion) {
            printVersion();
            System.exit(0);
        }

// System.out.println( "quiet = " + quiet );
// System.out.println( "showVersion = " + showVersion );
// System.out.println( "format = " + format );
// System.out.println( "store = " + store );

        try {
            execute(System.in, System.out, System.err);
        }

        catch (Throwable t) {
            System.out.println("Exited with error: " + t);
            t.printStackTrace();
            System.exit(1);
        }

        // Exit despite any remaining active threads.
        System.exit(0);
    }

    private static void execute(final InputStream in,
                                final PrintStream out,
                                final PrintStream err) {

    }

    private static void doImport(final Args args) throws Exception {
        String dir = args.nonOptions.get(0);
        String file = args.nonOptions.get(1);

        String f = args.pairs.get("f");
        RDFFormat format = (null == f) ? RDFFormat.forFileName(file, RDFFormat.RDFXML) : findRDFFormat(f);

        importRDFDocumentIntoNativeStore(new File(dir), new File(file), format);
    }

    private static void doDump(final Args args) throws Exception {
        String dir = args.nonOptions.get(0);
        String file = args.nonOptions.get(1);

        String f = args.pairs.get("f");
        RDFFormat format = (null == f) ? RDFFormat.forFileName(file, RDFFormat.RDFXML) : findRDFFormat(f);

        dumpNativeStoreToRDFDocument(new File(dir), new File(file), format);
    }

    public static void dumpNativeStoreToRDFDocument(final File nativeStoreDirectory,
                                                    final File dumpFile,
                                                    final RDFFormat format) throws SailException, RepositoryException, IOException, RDFHandlerException {
        System.out.println("dumping store at " + nativeStoreDirectory + " to file " + dumpFile);
        
        Sail sail = new NativeStore(nativeStoreDirectory);
        sail.initialize();

        try {
            Repository repo = new SailRepository(sail);

            RepositoryConnection rc = repo.getConnection();
            try {
                OutputStream out = new FileOutputStream(dumpFile);
                try {
                    RDFHandler h = Rio.createWriter(format, out);
                    rc.export(h);
                } finally {
                    out.close();
                }
            } finally {
                rc.close();
            }
        } finally {
            sail.shutDown();
        }
    }

    public static void importRDFDocumentIntoNativeStore(final File nativeStoreDirectory,
                                                        final File dumpFile,
                                                        final RDFFormat format) throws SailException, RepositoryException, IOException, RDFParseException {
        System.out.println("importing file " + dumpFile + " into store at " + nativeStoreDirectory);
        Sail sail = new NativeStore(nativeStoreDirectory);
        sail.initialize();

        try {
            Repository repo = new SailRepository(sail);

            RepositoryConnection rc = repo.getConnection();
            try {
                rc.add(dumpFile, "nobaseuri", format);
                rc.commit();
            } finally {
                rc.close();
            }
        } finally {
            sail.shutDown();
        }
    }
}
