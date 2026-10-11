package p;

abstract class BasePhased implements Phased {
	private final Phase phase;

	BasePhased(Phase phase) {
		this.phase = phase;
	}

	public Phase getPhase() {
		return this.phase;
	}
}
