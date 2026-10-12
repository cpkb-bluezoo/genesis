/**
 * Regression: component-less records still need equals/hashCode/toString
 * (DockerCliInspectResponse.ExposedPort).
 */
public class EmptyRecordEqualsVerifyTest {
    record ExposedPort() {
    }

    public static void main(String[] args) {
        ExposedPort a = new ExposedPort();
        ExposedPort b = new ExposedPort();
        if (!a.equals(b) || a.hashCode() != b.hashCode() || !a.toString().contains("ExposedPort")) {
            throw new RuntimeException("empty record methods");
        }
        System.out.println("EmptyRecordEqualsVerifyTest passed!");
    }
}
