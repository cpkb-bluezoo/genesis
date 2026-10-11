/*
 * Regression test: classes had no SourceFile attribute, so a stack trace showed "Unknown Source"
 * for every frame (and tests that compare traces, like Spring Boot's StandardStackTracePrinter's,
 * failed). The frame names the file, without its directory, and a line.
 */
public class SourceFileAttributeTest {

    static class Nested {
        StackTraceElement here() {
            return new Throwable().getStackTrace()[0];
        }
    }

    static StackTraceElement top() {
        return new Throwable().getStackTrace()[0];
    }

    public static void main(String[] args) {
        StackTraceElement[] frames = {top(), new Nested().here(), new Object() {
            StackTraceElement frame() {
                return new Throwable().getStackTrace()[0];
            }
        }.frame()};
        for (StackTraceElement frame : frames) {
            if (!"SourceFileAttributeTest.java".equals(frame.getFileName()) || frame.getLineNumber() <= 0) {
                throw new RuntimeException("FAIL: " + frame);
            }
        }
        System.out.println("PASS");
    }
}
