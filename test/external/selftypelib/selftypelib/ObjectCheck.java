package selftypelib;

public class ObjectCheck<A> extends Check<ObjectCheck<A>, A> {
	public ObjectCheck(A actual, StringBuilder log) {
		super(actual, log);
	}
}
