package p;

public class Srv {
	final StringBuilder log = new StringBuilder();

	public Srv option(String name) {
		log.append(name).append(';');
		return this;
	}

	public String log() {
		return log.toString();
	}
}
