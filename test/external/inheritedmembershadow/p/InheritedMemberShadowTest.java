package p;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;

/*
 * Regression test: a member type inherited through two levels of supertypes ("Entry" of
 * Details, via SubDetails) that lives in another file of the compile was shadowed by the
 * nested type of a class the file imports (LinkedHashMap's package-private Entry). The
 * method returned Stream<LinkedHashMap$Entry> and failed with IllegalAccessError at run time.
 * Compiled from this file alone with the others on the sourcepath.
 */
public class InheritedMemberShadowTest implements SubDetails {

    private final Map<String, Object> values = new LinkedHashMap<>();

    @Override
    public Stream<Entry> stream() {
        return this.values.entrySet().stream().map((entry) -> new Entry(entry.getKey(), entry.getValue()));
    }

    public static void main(String[] args) throws Exception {
        InheritedMemberShadowTest test = new InheritedMemberShadowTest();
        test.values.put("a", 1);
        if (test.stream().count() != 1) {
            throw new RuntimeException("FAIL: count");
        }
        String type = InheritedMemberShadowTest.class.getMethod("stream").getGenericReturnType().getTypeName();
        if (!type.endsWith("Details$Entry>")) {
            throw new RuntimeException("FAIL: " + type);
        }
        System.out.println("PASS");
    }
}
