package wcimport;

/*
 * ConnectHandler is visible here ONLY via the wildcard import below -
 * matching gumdrop's own MqttProtocolHandler.java (package
 * org.bluezoo.gumdrop.mqtt), whose "ConnectHandler" parameter type is
 * likewise visible only via "import org.bluezoo.gumdrop.mqtt.server.*;".
 */
import wcimport.server.*;

public class Handler {
    private ConnectHandler connectHandler;

    public void setConnectHandler(ConnectHandler h) {
        this.connectHandler = h;
    }

    public void fire() {
        if (connectHandler != null) {
            connectHandler.connect();
        }
    }
}
