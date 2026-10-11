package p;

/**
 * A member type inherited from a superclass, named through the SUBclass
 * ("Sub.Mode"), in a declaration and in an expression.
 */
public class InheritedMemberTypeQualifiedTest {
	private Sub.Mode mode = Sub.Mode.SIMPLE;

	private Sub.Settings settings = new Sub.Settings();

	public static void main(String[] args) {
		InheritedMemberTypeQualifiedTest test = new InheritedMemberTypeQualifiedTest();
		Base.Mode viaBase = test.mode;
		Sub.Mode viaSub = Base.Mode.FULL;
		test.mode = viaSub;
		if (viaBase != Base.Mode.SIMPLE || test.mode != Sub.Mode.FULL || !"base".equals(test.settings.name)) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
