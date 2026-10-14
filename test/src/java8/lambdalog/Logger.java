package lambdalog;

public interface Logger {
    void debug(Message message);

    void debug(MessageSupplier supplier);

    void debug(CharSequence message);

    void debug(Object message);

    void debug(String message);

    void debug(Supplier<?> supplier);
}
