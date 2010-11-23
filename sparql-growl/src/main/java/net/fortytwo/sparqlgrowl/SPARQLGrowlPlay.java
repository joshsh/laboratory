package net.fortytwo.sparqlgrowl;

import net.sf.libgrowl.Application;
import net.sf.libgrowl.GrowlConnector;
import net.sf.libgrowl.Notification;
import net.sf.libgrowl.NotificationType;

This code will not work until Growl supports GNTP.  This is said to be a major feature of Growl 1.3. Check for new releases here:
    http://code.google.com/p/growl/downloads/list


/**
 * User: josh
 * Date: Nov 23, 2010
 * Time: 5:55:53 PM
 */
public class SPARQLGrowlPlay {
    public static void main(final String[] args) throws Exception {
        /*
// connect to Growl on the given host
        GrowlConnector growl = new GrowlConnector("localhost");

// give your application a name and icon (optionally)
        Application app = new Application("SPARQL-Growl test app");

// create reusable notification types, their names are used in the Growl settings
        NotificationType resultsType = new NotificationType("You've got SPARQL results!");
        NotificationType[] notificationTypes = new NotificationType[]{resultsType};

// now register the application in growl
        growl.register(app, notificationTypes);

// create a notification with specific title and message
        Notification ubuntuDownload = new Notification(app, resultsType, "Ubuntu 9.4", "654 MB");

// finally send the notification
        growl.notify(ubuntuDownload);
        */

        // connect to Growl on the given host
        GrowlConnector growl = new GrowlConnector("localhost", 23052);

// give your application a name and icon (optionally)
        Application downloadApp = new Application("Downloader");

// create reusable notification types, their names are used in the Growl settings
        NotificationType downloadStarted = new NotificationType("Download started");
        NotificationType downloadFinished = new NotificationType("Download finished");
        NotificationType[] notificationTypes = new NotificationType[]{downloadStarted, downloadFinished};

// now register the application in growl
        growl.register(downloadApp, notificationTypes);

// create a notification with specific title and message
        Notification ubuntuDownload = new Notification(downloadApp, downloadStarted, "Ubuntu 9.4", "654 MB");

// finally send the notification
        growl.notify(ubuntuDownload);
    }
}
