package spdep;

/* Library half of the -sourcepath tests (see Handler): never referenced by
 * any test source. javac compiles only the files a compilation actually
 * reaches, so no Unrelated.class may be written. Its body is deliberately
 * not a type name used elsewhere. */
public class Unrelated {
    int x = 1;
}
