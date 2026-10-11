package p;

public class Builder {
	private final StringBuilder names = new StringBuilder();

	public Builder addCustom(Conv<?> conv) {
		this.names.append(conv.name()).append(';');
		return this;
	}

	public String names() {
		return this.names.toString();
	}
}
