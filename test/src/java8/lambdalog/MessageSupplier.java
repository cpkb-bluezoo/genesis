package lambdalog;

public interface MessageSupplier extends Supplier<Message> {
    Message get();
}
