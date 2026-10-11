import java.util.HashMap;
import java.util.Map;

public class NestedClassBoundedMethodTypeVariableTest {
	interface Configuration<K, V> {
		String describe();
	}

	interface Named {
		String name();
	}

	static class Provider {
		final Map<String, Configuration<?, ?>> configurations = new HashMap<String, Configuration<?, ?>>();

		<K, V, C extends Configuration<K, V>> String create(String cacheName, C configuration) {
			this.configurations.put(cacheName, configuration);
			Configuration<?, ?> same = configuration;
			return configuration.describe() + same.describe();
		}

		<N extends Named> String show(N named) {
			Named base = named;
			return base.name() + named.name();
		}
	}

	public static void main(String[] args) {
		Provider provider = new Provider();
		Configuration<String, String> configuration = new Configuration<String, String>() {
			public String describe() {
				return "cfg";
			}
		};
		Named named = new Named() {
			public String name() {
				return "n";
			}
		};
		String created = provider.create("c", configuration);
		String shown = provider.show(named);
		if (!"cfgcfg".equals(created) || !"nn".equals(shown) || provider.configurations.size() != 1) {
			throw new AssertionError(created + shown);
		}
		System.out.println("PASS");
	}
}
