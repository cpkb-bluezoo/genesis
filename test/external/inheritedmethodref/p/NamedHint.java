package p;

public abstract class NamedHint {
	private final String name;

	protected NamedHint(String name) {
		this.name = name;
	}

	public String getName() {
		return this.name;
	}
}
