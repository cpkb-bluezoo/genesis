package lambdalog;

public interface Message {
    String getFormattedMessage();

    Object[] getParameters();
}
