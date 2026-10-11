/*
 * Regression test: "s += flag ? a : \".\" + b" - a String compound assignment whose right-hand
 * side branches - recorded the stack as [String, StringBuilder] at the branch target (the order
 * before the swap that puts the builder underneath): VerifyError at class load.
 */
public class StringCompoundAssignWithBranchTest {

    static String nested(String prefix, String name) {
        String result = (prefix != null) ? prefix : "";
        String dashed = name.toLowerCase();
        result += result.isEmpty() ? dashed : "." + dashed;
        return result;
    }

    public static void main(String[] args) {
        String actual = nested(null, "AB") + " " + nested("x", "CD");
        if (!"ab x.cd".equals(actual)) {
            throw new RuntimeException("FAIL: " + actual);
        }
        System.out.println("StringCompoundAssignWithBranchTest passed!");
    }
}
