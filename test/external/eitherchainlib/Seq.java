public interface Seq<T> extends Value<T> {
    Option<T> headOption();

    static <T> Seq<T> of(T head) {
        return () -> Option.some(head);
    }
}
