package inheritedmembertype.lexer;

import inheritedmembertype.base.Base;
import inheritedmembertype.base.Kind;
import inheritedmembertype.base.Tagged;

/* Regression test: a member type INHERITED from a superclass or
 * superinterface (JLS 8.5) and named by its simple name in another
 * class's constructor/method/field signature, where that class is
 * compiled in the same batch but read from a DIFFERENT file (this one).
 *
 * Lexer (Lexer.java) extends Base, which declares `interface Handler<T>`
 * and `class Options`; Deep (Deep.java) extends Mid, which extends Base
 * and implements Tagged (declaring `class Tag`). Their signatures say
 * just `Handler<Token>`, `Options`, `Tag`. The shared-registry stub that
 * cross-file callers read those signatures from had no rule for
 * inherited member types at all, so such parameter types stayed
 * unresolved (NULL), and codegen silently dropped each such parameter
 * from the call-site descriptor: `new Lexer(this, 10)` was emitted as
 * `Lexer.<init>:(I)V`, failing with "VerifyError: Bad operand type when
 * invoking <init>". Mirrors gumdrop's ZoneFileParser/ZoneFileLexer/
 * ByteStreamLexer (AuthoritativeZoneHandlerTest).
 *
 * MUST be compiled into a freshly emptied output directory (see
 * run-tests.sh): genesis puts its own -d directory on the classpath, and
 * class files left there by an earlier compile can mask this bug. */
public class InheritedMemberTypeVerifyTest implements Base.Handler<Lexer.Token> {

    private int seen;
    private Lexer.Token last;

    @Override
    public boolean token(Lexer.Token type, int n) {
        seen += n;
        last = type;
        return true;
    }

    static final class KindCounter implements Base.Handler<Kind> {
        int words;
        int numbers;

        @Override
        public boolean token(Kind type, int n) {
            if (type == Kind.WORD) {
                words += n;
            } else {
                numbers += n;
            }
            return true;
        }
    }

    private static void check(boolean ok, String what) {
        if (!ok) {
            throw new RuntimeException("failed: " + what);
        }
    }

    public static void main(String[] args) {
        InheritedMemberTypeVerifyTest t = new InheritedMemberTypeVerifyTest();

        /* Constructor: (Handler, int) - the gumdrop case */
        Lexer a = new Lexer(t, 10);
        check(a.fire(), "constructor fire");
        check(t.seen == 10 && t.last == Lexer.Token.ATOM, "constructor handler");
        check(a.options().limit == 10, "Options return type");

        /* Static and instance methods taking the inherited type */
        Lexer c = Lexer.make(5, t);
        check(c.fire() && t.seen == 15, "static method parameter");
        InheritedMemberTypeVerifyTest other = new InheritedMemberTypeVerifyTest();
        c.set(other, 99L);
        check(c.fire() && other.seen == 5 && t.seen == 15, "instance method parameter");

        /* Method return type and field, read and write */
        Base.Handler<Lexer.Token> got = c.get();
        check(got == other, "Handler return type");
        c.tokens = t;
        Base.Handler<Lexer.Token> field = c.tokens;
        check(field == t, "Handler field");
        Base.Options opts = c.options;
        check(opts.limit == 5, "Options field");

        /* Nested class naming a type its enclosing class inherits */
        Lexer.Probe p = new Lexer.Probe(t, 3);
        check(p.fire() && t.seen == 18 && t.last == Lexer.Token.TEXT, "nested class constructor");

        /* Transitive: through an intermediate superclass, and through
         * that superclass's superinterface */
        KindCounter k = new KindCounter();
        KindCounter extra = new KindCounter();
        Deep d = new Deep(k, new Base.Options(7), new Tagged.Tag("deep"));
        check(d.fire(extra), "transitive constructor fire");
        check(k.words == 7 && extra.numbers == 1, "transitive handlers");
        check("deep".equals(d.tag().name), "transitive Tag return type");
        check(d.options().limit == 7, "transitive Options return type");

        System.out.println("PASS");
    }
}
