import annoelementslib.Ann;
import annoelementslib.Ann.Mode;
import annoelementslib.Ann.Sub;

/* Regression test: every kind of annotation element value must be encoded
 * correctly (JVMS 4.7.16.1), both where an annotation is USED and in the
 * annotation type's own AnnotationDefault attributes. genesis encoded only
 * string, int and boolean constants and unqualified or one-level enum
 * constants; an array, a long/float/double/char constant, a negative
 * number, a qualified enum constant like Ann.Mode.A, a nested annotation or a
 * class array each wrote an element name with no value, so reading ANY
 * annotation on that class threw "AnnotationFormatError: Unexpected end of
 * annotations". Mirrors gumdrop's servlet tests (@WebFilter(urlPatterns =
 * {...}, dispatcherTypes = {...}), @MultipartConfig(maxFileSize = 5L)). The
 * annotation type is compiled FIRST so its class file supplies the declared
 * element types. */
public class AnnotationElementKindsTest {
    @Ann(paths = { "/a", "/b" }, modes = { Mode.A, Ann.Mode.C }, size = 5L)
    static class Arrays {
    }

    @Ann(paths = "/single", mode = Ann.Mode.B, size = -7L, count = 3, flag = true)
    static class Singles {
    }

    @Ann(ratio = 2.25, weight = 3.5f, ch = 'q', b = 9, sh = 300, type = String.class,
            types = { Integer.class, int[].class })
    static class Numbers {
    }

    @Ann(nested = @Sub("n"), subs = { @Sub("s1"), @Sub })
    static class Nested {
    }

    @Ann
    static class Defaults {
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new RuntimeException("failed: " + what);
        }
    }

    public static void main(String[] args) {
        Ann a = Arrays.class.getAnnotation(Ann.class);
        check(a.paths().length == 2 && "/b".equals(a.paths()[1]), "string array");
        check(a.modes().length == 2 && a.modes()[0] == Mode.A && a.modes()[1] == Mode.C, "enum array");
        check(a.size() == 5L, "long");

        a = Singles.class.getAnnotation(Ann.class);
        check(a.paths().length == 1 && "/single".equals(a.paths()[0]), "single value for array");
        check(a.mode() == Mode.B, "qualified enum");
        check(a.size() == -7L, "negative long");
        check(a.count() == 3 && a.flag(), "int and boolean");

        a = Numbers.class.getAnnotation(Ann.class);
        check(a.ratio() == 2.25 && a.weight() == 3.5f, "double and float");
        check(a.ch() == 'q' && a.b() == 9 && a.sh() == 300, "char, byte, short");
        check(a.type() == String.class, "class");
        check(a.types().length == 2 && a.types()[0] == Integer.class && a.types()[1] == int[].class,
                "class array");

        a = Nested.class.getAnnotation(Ann.class);
        check("n".equals(a.nested().value()), "nested annotation");
        check(a.subs().length == 2 && "s1".equals(a.subs()[0].value())
                && "sub-default".equals(a.subs()[1].value()), "annotation array");

        a = Defaults.class.getAnnotation(Ann.class);
        check(a.mode() == Mode.A && a.modes().length == 1 && a.modes()[0] == Mode.B, "enum defaults");
        check(a.size() == -1L && a.ratio() == 0.5 && a.weight() == 1.5f && a.ch() == 'x', "numeric defaults");
        check(a.b() == 1 && a.sh() == 2 && a.type() == Object.class && a.types().length == 0, "other defaults");
        check("d".equals(a.nested().value()), "nested default");
        System.out.println("AnnotationElementKindsTest passed!");
    }
}
