package p;

public interface Phased {
	enum Phase { PARSE, REGISTER }

	Phase getPhase();
}
