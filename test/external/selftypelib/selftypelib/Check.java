package selftypelib;

import java.util.function.Consumer;

public abstract class Check<SELF extends Check<SELF, A>, A> {
	final A actual;
	final StringBuilder log;

	protected Check(A actual, StringBuilder log) {
		this.actual = actual;
		this.log = log;
	}

	@SuppressWarnings("unchecked")
	public SELF has(String name, Object value) {
		this.log.append(name).append('=').append(value).append(';');
		return (SELF) this;
	}

	public Check<?, ?> extracting(String name) {
		this.log.append("extract ").append(name).append(';');
		return new ObjectCheck<Object>(this.actual, this.log);
	}

	public <T> SELF satisfies(Class<T> type, Consumer<T> requirements) {
		requirements.accept(type.cast(this.actual));
		return self();
	}

	@SuppressWarnings("unchecked")
	public SELF self() {
		return (SELF) this;
	}
}
