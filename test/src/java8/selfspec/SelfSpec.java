package selfspec;

/** Javac-compiled stand-in for WebTestClient.MockServerSpec: <T extends B> T apply. */
public interface SelfSpec<B extends SelfSpec<B>> {
    <T extends B> T apply(String value);
}
