package inheritedmembertype.lexer;

import inheritedmembertype.base.Base;

/* Helper for InheritedMemberTypeVerifyTest: every signature here names an
 * INHERITED member type (Handler, Options) by its simple name, from a
 * different package than the one declaring it. Mirrors gumdrop's
 * ZoneFileLexer exactly (`final class ZoneFileLexer extends
 * ByteStreamLexer<ZoneFileLexer.Token>`, with a constructor
 * `ZoneFileLexer(Handler<Token> handler, int maxTokenLength)`). */
final class Lexer extends Base<Lexer.Token> {

    enum Token {
        ATOM,
        TEXT
    }

    /* A nested class sees the member types its ENCLOSING class inherits. */
    static final class Probe {
        final Handler<Token> target;
        final int weight;

        Probe(Handler<Token> target, int weight) {
            this.target = target;
            this.weight = weight;
        }

        boolean fire() {
            return target.token(Token.TEXT, weight);
        }
    }

    Handler<Token> tokens;
    Options options;

    Lexer(Handler<Token> handler, int max) {
        super(handler, max);
        this.tokens = handler;
        this.options = new Options(max);
    }

    static Lexer make(int max, Handler<Token> handler) {
        return new Lexer(handler, max);
    }

    void set(Handler<Token> handler, long pad) {
        this.tokens = handler;
    }

    Handler<Token> get() {
        return tokens;
    }

    Options options() {
        return options;
    }

    boolean fire() {
        return tokens.token(Token.ATOM, max);
    }
}
