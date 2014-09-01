
////////////////////////////////////////////////////////////////////////////////

#include <OSCBundle.h>
#include <ExtendOSC.h>

ExtendOSC osc("/exo/test");

OSCBundle *bundleIn;

void setup() {
    osc.beginSerial();
    
    bundleIn = new OSCBundle();   
}

void handleOSCBundle(class OSCBundle &bundle) {
    if (bundle.hasError()) {
        osc.sendOSCBundleError(bundle);
    } else {
        boolean called = 0
        || bundle.dispatch("/exo/test/ping", handlePingMessage)
        ;
    }
}

void handlePingMessage(class OSCMessage &mIn) {
    OSCMessage mOut("/exo/test/ping-reply");
    mOut.add((uint64_t) millis());
    osc.sendOSC(mOut); 
}

void sendHeartbeatMessage(unsigned long now) {
    OSCMessage m("/exo/test/heartbeat");
    m.add((uint64_t) now);
    osc.sendOSC(m); 
}

void sendReply(int arg) {
//void sendReply(const char* arg) {
    OSCMessage m("/exo/test/reply");
    m.add(arg);
    osc.sendOSC(m); 
}

unsigned long lastHeartbeat = 0;

void loop() {
    unsigned long now = millis();
    
    if (now - lastHeartbeat > 1000) {
        sendHeartbeatMessage(now);
        lastHeartbeat = now;  
    }
    
    if (osc.receiveOSCBundle(*bundleIn)) {
        //sendReply(bundleIn->size());
        handleOSCBundle(*bundleIn);
        bundleIn->empty();
        delete bundleIn;    // TODO: possible to bundleIn->empty() instead of deleting?
        bundleIn = new OSCBundle();
    }
}
