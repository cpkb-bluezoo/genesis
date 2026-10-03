import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;

/* Regression test: "return <expr>;" inside a try-finally. The value is parked
 * in a temp local across the inlined finally body and reloaded before the
 * return; the temp was declared as Object, so the stack-map frame at the
 * finally's join typed the reloaded value as Object and the areturn failed
 * verification ("Type 'java/lang/Object' is not assignable to '...'").
 * Mirrors gumdrop's TaglibRegistry.loadTldFromLocation() and
 * Pop3ProtocolHandler's byte[] call(). */
public class ReturnThroughFinallyTypedVerifyTest {
    static final class Doc {
        final String name;

        Doc(String name) {
            this.name = name;
        }
    }

    static Doc parse(InputStream in, String name) {
        return new Doc(name);
    }

    static Doc load(String location, InputStream stream) throws IOException {
        InputStream in = null;
        try {
            if (location.startsWith("jar:")) {
                return null;
            } else {
                in = stream;
            }
            if (in != null) {
                return parse(in, location);
            }
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    location = "closed-failed";
                }
            }
        }
        return null;
    }

    static byte[] bytes(byte[] src) throws IOException {
        ByteArrayInputStream in = new ByteArrayInputStream(src);
        try {
            return src.clone();
        } finally {
            in.close();
        }
    }

    public static void main(String[] args) throws IOException {
        Doc d = load("/x.tld", new ByteArrayInputStream(new byte[0]));
        if (d == null || !"/x.tld".equals(d.name)) {
            throw new RuntimeException("doc");
        }
        if (load("jar:foo", null) != null || load("/y", null) != null) {
            throw new RuntimeException("null paths");
        }
        byte[] b = bytes(new byte[] { 1, 2, 3 });
        if (b.length != 3 || b[2] != 3) {
            throw new RuntimeException("bytes");
        }
        System.out.println("ReturnThroughFinallyTypedVerifyTest passed!");
    }
}
