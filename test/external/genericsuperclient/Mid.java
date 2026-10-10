import genericsuperlib.Base;

/* Package-private middle link of the chain, in its own file (the failure only
 * shows when the chain is split across files). */
abstract class Mid<E extends Mid<E>> extends Base<E> {
    String name = "m";

    E name(String n) {
        name = n;
        return self();
    }
}
