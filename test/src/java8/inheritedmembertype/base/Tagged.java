package inheritedmembertype.base;

/* Helper for InheritedMemberTypeVerifyTest: a member type (Tag) inherited
 * through a superINTERFACE rather than a superclass.
 *
 * Tag spells out "public static" although both are implicit for a member
 * type of an interface (JLS 9.5): genesis does not yet apply the implicit
 * "public" to the emitted class, which is a separate bug from the one
 * this test covers and would fail it with an IllegalAccessError. */
public interface Tagged {

    public static final class Tag {
        public final String name;

        public Tag(String name) {
            this.name = name;
        }
    }

    Tag tag();
}
