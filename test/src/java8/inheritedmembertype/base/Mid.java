package inheritedmembertype.base;

/* Helper for InheritedMemberTypeVerifyTest: an intermediate class that
 * declares no member types of its own, so everything a subclass inherits
 * through it comes TRANSITIVELY - Handler/Options from its superclass
 * (Base), Tag from its superinterface (Tagged). */
public abstract class Mid extends Base<Kind> implements Tagged {

    protected Mid(Handler<Kind> handler, int max) {
        super(handler, max);
    }
}
