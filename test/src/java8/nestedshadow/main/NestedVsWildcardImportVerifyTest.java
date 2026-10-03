package nestedshadow.main;

import nestedshadow.lib.*;
import java.nio.file.DirectoryStream;

/* Regression test: a bare type name that is BOTH a top-level type reachable
 * through an on-demand (wildcard) import and a member type of an unrelated
 * single-type-imported class must resolve to the on-demand-imported type.
 * Importing DirectoryStream does NOT bring DirectoryStream.Filter into scope
 * (JLS 6.4.1), so "Filter" here is nestedshadow.lib.Filter.
 *
 * Mirrors gumdrop's servlet Context.java: "import java.nio.file.DirectoryStream;
 * import jakarta.servlet.*;" then "Filter filter = ...", which genesis
 * resolved to DirectoryStream.Filter ("cannot convert jakarta.servlet.Filter
 * to java.nio.file.DirectoryStream$Filter"). resolve_import() (semantic.c)
 * scanned the members of every single-type-imported class before
 * consulting any on-demand import. */
public class NestedVsWildcardImportVerifyTest {
    static Filter make() {
        return new Filter() {
            public String name() {
                return "lib";
            }
        };
    }

    public static void main(String[] args) {
        Filter f = make();
        if (!"lib".equals(f.name())) {
            throw new RuntimeException("wrong Filter: " + f.name());
        }
        DirectoryStream.Filter<String> df = new DirectoryStream.Filter<String>() {
            public boolean accept(String s) {
                return true;
            }
        };
        if (df == null) {
            throw new RuntimeException("unreachable");
        }
        System.out.println("PASS");
    }
}
