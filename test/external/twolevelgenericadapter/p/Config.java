package p;

public interface Config {
	default boolean enabled() {
		return false;
	}

	default int steps() {
		return 7;
	}
}
