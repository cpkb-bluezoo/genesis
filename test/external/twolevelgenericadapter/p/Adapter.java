package p;

public class Adapter extends StepAdapter<Props> implements Config {
	public Adapter() {
		super(new Props());
	}

	@Override
	public boolean enabled() {
		return obtain(Props::isLenient, Config.super::enabled);
	}

	@Override
	public int steps() {
		return obtain(Props::getSteps, Config.super::steps);
	}
}
