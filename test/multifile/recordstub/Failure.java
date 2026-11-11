public sealed interface Failure {
    record Transport(String message, Throwable cause) implements Failure {}
    record Timeout(long millis, int[] attempts, java.util.List<String> hosts) implements Failure {}
    record Holder<T extends Number>(T value, T[] more) implements Failure {}
}
