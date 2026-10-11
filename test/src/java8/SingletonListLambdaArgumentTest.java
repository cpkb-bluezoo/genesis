import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class SingletonListLambdaArgumentTest {
	interface Customizer {
		void customize(StringBuilder server);
	}

	static final StringBuilder log = new StringBuilder();

	static void setCustomizers(Collection<? extends Customizer> customizers) {
		for (Customizer customizer : customizers) {
			customizer.customize(log);
		}
	}

	static void setList(List<Customizer> customizers) {
		for (Customizer customizer : customizers) {
			customizer.customize(log);
		}
	}

	public static void main(String[] args) {
		setCustomizers(Collections.singletonList((server) -> server.append("one;")));
		setList(Collections.singletonList((server) -> server.append("two;")));
		if (!"one;two;".equals(log.toString())) {
			throw new AssertionError(log.toString());
		}
		System.out.println("PASS");
	}
}
