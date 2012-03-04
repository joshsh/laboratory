package net.fortytwo.laboratory.rfid;

import com.alien.enterpriseRFID.discovery.AlienDiscoveryUnknownReaderException;
import com.alien.enterpriseRFID.discovery.DiscoveryItem;
import com.alien.enterpriseRFID.discovery.DiscoveryListener;
import com.alien.enterpriseRFID.discovery.NetworkDiscoveryListenerService;
import com.alien.enterpriseRFID.reader.AlienClass1Reader;

public class NetworkDiscoverer implements DiscoveryListener {

    private AlienClass1Reader reader;

    public NetworkDiscoverer() throws Exception {
        NetworkDiscoveryListenerService service = new NetworkDiscoveryListenerService();
        service.setDiscoveryListener(this);
        service.startService();

        // Spin while readers are discovered.
        while (null == reader) {
            Thread.sleep(100);
        }
    }

    public AlienClass1Reader getReader() {
        return reader;
    }

    public void readerAdded(DiscoveryItem discoveryItem) {
        try {
            reader = discoveryItem.getReader();
        } catch (AlienDiscoveryUnknownReaderException e) {
            System.err.println("error while getting reader");
            e.printStackTrace(System.err);
        }
    }

    public void readerRenewed(DiscoveryItem discoveryItem) {
        // Do nothing.
    }

    public void readerRemoved(DiscoveryItem discoveryItem) {
        // Do nothing.
    }
}