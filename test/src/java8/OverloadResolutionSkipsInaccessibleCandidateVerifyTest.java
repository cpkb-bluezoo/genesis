/**
 * Bug: overload resolution picked a MORE SPECIFIC but INACCESSIBLE
 * candidate method over a less-specific, accessible one, then rejected
 * the whole call with a spurious compile error - instead of excluding
 * the inaccessible candidate from consideration in the first place, as
 * JLS 15.12.2.1 ("Identify Potentially Applicable Methods") requires.
 *
 * java.lang.StringBuffer declares a package-private
 * "append(AbstractStringBuilder)" overload (an internal fast path
 * shared between StringBuilder/StringBuffer) alongside the public
 * "append(CharSequence)". AbstractStringBuilder (package-private
 * itself) is a subtype of CharSequence, so for a StringBuilder-typed
 * argument, "append(AbstractStringBuilder)" scores as the more
 * specific match by parameter type alone - but it's not accessible from
 * outside java.lang. find_best_method_by_types() (semantic.c) picked it
 * anyway, and a separate, pre-existing accessibility check on the
 * CHOSEN method (not the candidate set) then correctly flagged it as
 * inaccessible and refused to compile - even though the accessible
 * "append(CharSequence)" overload was right there. Confirmed against
 * gumdrop's own HttpDateFormat.format(), whose "buf.append(sb)"
 * (appending a StringBuilder into a StringBuffer) hit exactly this.
 */
public class OverloadResolutionSkipsInaccessibleCandidateVerifyTest {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder("hello");
        StringBuffer buf = new StringBuffer();
        buf.append(sb);
        String result = buf.toString();
        if (!"hello".equals(result)) {
            throw new RuntimeException("expected 'hello', got '" + result + "'");
        }
    }
}
