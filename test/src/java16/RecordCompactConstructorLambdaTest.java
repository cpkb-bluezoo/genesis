import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class RecordCompactConstructorLambdaTest {
	static void each(Consumer<List<Object>> consumer, List<Object> into) {
		consumer.accept(into);
	}

	record Config(String first, String second) {
		static final List<Object> SEEN = new ArrayList<Object>();

		Config {
			if (first == null && second == null) {
				throw new IllegalArgumentException("missing");
			}
			each((entries) -> {
				entries.add(first);
				entries.add(second);
			}, SEEN);
		}
	}

	record Child(List<Config> child, String name) {
		Child {
			child.forEach(this::check);
		}

		private void check(Config config) {
			if (config.first() == null && config.second() == null) {
				throw new IllegalStateException("bad");
			}
		}
	}

	public static void main(String[] args) {
		Config config = new Config("a", null);
		if (!"a".equals(config.first()) || config.second() != null || Config.SEEN.size() != 2) {
			throw new AssertionError("config " + config + " " + Config.SEEN);
		}
		List<Config> configs = new ArrayList<Config>();
		configs.add(config);
		Child child = new Child(configs, "n");
		if (!"n".equals(child.name()) || child.child().size() != 1) {
			throw new AssertionError("child");
		}
		System.out.println("PASS");
	}
}
