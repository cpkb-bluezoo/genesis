package inheritedmembertype.lexer;

import inheritedmembertype.base.Kind;
import inheritedmembertype.base.Mid;

/* Helper for InheritedMemberTypeVerifyTest: the member types named here
 * are inherited TRANSITIVELY - Mid (the only supertype this file names or
 * imports) declares none of them itself. Handler and Options come from
 * Mid's superclass, Tag from Mid's superinterface. */
final class Deep extends Mid {

    private final Options options;
    private final Tag tag;

    Deep(Handler<Kind> handler, Options options, Tag tag) {
        super(handler, options.limit);
        this.options = options;
        this.tag = tag;
    }

    Options options() {
        return options;
    }

    @Override
    public Tag tag() {
        return tag;
    }

    boolean fire(Handler<Kind> extra) {
        return handler.token(Kind.WORD, max) && extra.token(Kind.NUMBER, 1);
    }
}
