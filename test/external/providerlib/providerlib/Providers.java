package providerlib;

public final class Providers {
    private Providers() {
    }

    public static <T> T first(Provider<T> provider) {
        return provider.get();
    }
}
