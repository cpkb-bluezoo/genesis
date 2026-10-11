package p;

import java.util.function.Supplier;

public class ConfigAdapter<T> {
	private final T properties;

	public ConfigAdapter(T properties) {
		this.properties = properties;
	}

	protected final T get() {
		return this.properties;
	}

	protected final <V> V obtain(Getter<T, V> getter, Supplier<V> fallback) {
		V value = getter.get(this.properties);
		return (value != null) ? value : fallback.get();
	}

	@FunctionalInterface
	protected interface Getter<T, V> {
		V get(T properties);
	}
}
