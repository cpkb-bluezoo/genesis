package crossfileenummethodname;

public class Metrics {
    private String lastType;
    private String lastProto;

    public void queryReceived(String type, String proto) {
        lastType = type;
        lastProto = proto;
    }

    public String getLastType() {
        return lastType;
    }

    public String getLastProto() {
        return lastProto;
    }
}
