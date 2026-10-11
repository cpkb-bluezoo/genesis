package p;

import java.util.function.Consumer;

public class Holder<T> {
	private final T value;

	public Holder(T value) {
		this.value = value;
	}

	public void ifPresent(Consumer<? super T> consumer) {
		consumer.accept(this.value);
	}

	public void each(Consumer<T> consumer) {
		consumer.accept(this.value);
	}
}
