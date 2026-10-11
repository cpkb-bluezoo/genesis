import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class ExplicitTypeArgumentLambdaTest {
	interface Profiles {
		boolean matches(Predicate<String> activeProfiles);
	}

	static <T> T assertArg(Consumer<T> consumer) {
		return null;
	}

	static <T> T apply(T value, Consumer<T> consumer) {
		consumer.accept(value);
		return value;
	}

	static boolean accepts(Profiles profiles) {
		return profiles != null;
	}

	public static void main(String[] args) {
		final List<String> names = new ArrayList<String>();
		accepts(ExplicitTypeArgumentLambdaTest.<Profiles>assertArg(((profiles) -> {
			profiles.matches((profile) -> {
				names.add(profile);
				return false;
			});
		})));
		Profiles all = (p) -> p.test("a") | p.test("b");
		ExplicitTypeArgumentLambdaTest.<Profiles>apply(all, (profiles) -> {
			profiles.matches((profile) -> {
				names.add(profile);
				return false;
			});
		});
		if (names.size() != 2) {
			throw new AssertionError("expected 2 names, got " + names);
		}
		System.out.println("PASS");
	}
}
