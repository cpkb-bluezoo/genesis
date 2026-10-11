/*
 * Regression test: a class declared in an interface is implicitly static (JLS 9.5), but
 * "new Members<>(...)" from another member class of the same interface was compiled as
 * the creation of an inner class (an enclosing instance pushed as a constructor argument):
 * VerifyError at class load.
 */
import java.util.function.*;
interface MemberClassOfInterfaceHolder<T> {
	final class Members<T> {
		final Consumer<Members<T>> c; final boolean b;
		Members(Consumer<Members<T>> c, boolean b) { this.c = c; this.b = b; }
	}
	final class Member<T> {
		Members<T> members; private final String name;
		Member(String n) { name = n; }
		public Member<T> usingMembers(Consumer<Members<T>> members) {
			this.members = new Members<>(members, this.name == null);
			return this;
		}
	}
}

public class MemberClassOfInterfaceTest {
    public static void main(String[] args) {
        MemberClassOfInterfaceHolder.Member<String> member = new MemberClassOfInterfaceHolder.Member<String>(null);
        if (!member.usingMembers(m -> { }).members.b) {
            throw new RuntimeException("FAIL: members not created");
        }
        System.out.println("MemberClassOfInterfaceTest passed!");
    }
}
