package p;

/**
 * A member type (an enum) of an interface that a SUPERCLASS's superclass
 * implements, named by its simple name: in a super(...) call and in a body.
 */
public class SuperInterfaceMemberTypeTest {
	static class Mine extends AnyPhased {
		Mine() {
			super(Phase.REGISTER);
		}

		Phase other() {
			return Phase.PARSE;
		}
	}

	public static void main(String[] args) {
		Mine mine = new Mine();
		if (mine.getPhase() != Phased.Phase.REGISTER || mine.other() != Phased.Phase.PARSE) {
			throw new AssertionError("wrong phase");
		}
		System.out.println("PASS");
	}
}
