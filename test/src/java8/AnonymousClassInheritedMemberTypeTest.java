import java.util.Map.Entry;

public class AnonymousClassInheritedMemberTypeTest {
	interface Details {
		/* shadows the imported Map.Entry inside everything that implements Details */
		interface Entry {
			static Entry of(String value) {
				return () -> value;
			}

			String value();
		}

		Entry getEntry();
	}

	static Entry<String, String> imported() {
		return new java.util.AbstractMap.SimpleEntry<String, String>("k", "v");
	}

	public static void main(String[] args) {
		Details details = new Details() {
			@Override
			public Entry getEntry() {
				return Entry.of("inner");
			}
		};
		if (!"inner".equals(details.getEntry().value()) || !"k".equals(imported().getKey())) {
			throw new AssertionError("wrong");
		}
		System.out.println("PASS");
	}
}
