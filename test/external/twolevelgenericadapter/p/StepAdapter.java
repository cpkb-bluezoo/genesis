package p;

public class StepAdapter<T extends StepProperties> extends ConfigAdapter<T> {
	public StepAdapter(T properties) {
		super(properties);
	}

	public String prefix() {
		return obtain(StepProperties::getPrefix, () -> "none");
	}
}
